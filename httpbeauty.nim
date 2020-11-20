import asyncdispatch, cgi, httpbeast, httpcore, json, options, parseutils, strtabs, strutils, tables, uri
import redis except `%`


type
  RawHeaders* = seq[tuple[key, val: string]]

  ResponseData* = tuple[
    code: HttpCode,
    headers: Option[RawHeaders],
    content: string,
  ]

  MultiData* = OrderedTable[string, tuple[fields: StringTableRef, body: string]]

  RouteProc*[T] = proc (request: Request, ctx: T): Future[Option[ResponseData]] {.gcsafe, locks:0.}

proc createHeaders(headers: RawHeaders): string =
  result = ""
  if headers.len > 0:
    for header in headers:
      let (key, value) = header
      result.add(key & ": " & value & "\c\L")

    result = result[0 .. ^3] # Strip trailing \c\L

proc handleRequest*[T](routers: seq[RouteProc[T]], req: Request, ctx: T): Future[void] {.async, gcsafe.} =
  for router in routers:
    let repOpt = await router(req, ctx)
    if repOpt.isSome:
      let
        (code, headers, content) = repOpt.get
        h =
          if headers.isNone: ""
          else: headers.get().createHeaders
      send(req, code, content, h)
      return
  send(req, Http404, "")

template setHeader(headers: var Option[RawHeaders], key, value: string): void =
  bind isNone
  if isNone(headers):
    headers = some(@({key: value}))
  else:
    block outer:
      # Overwrite key if it exists.
      var h = headers.get()
      for i in 0 ..< h.len:
        if h[i][0] == key:
          h[i][1] = value
          headers = some(h)
          break outer

      # Add key if it doesn't exist.
      headers = some[RawHeaders](h & @({key: value}))

template resp*(code: HttpCode,
               headers: openarray[tuple[key, val: string]],
               content: string): void =
  ## Sets ``(code, headers, content)`` as the response.
  var ret = (code, none[RawHeaders](), content)
  for header in headers:
    setHeader(ret[1], header[0], header[1])
  result = some[ResponseData](ret)

template resp*(content: string, contentType = "text/html;charset=utf-8"): void =
  ## Sets ``content`` as the response; ``Http200`` as the status code
  ## and ``contentType`` as the Content-Type.
  resp(Http200, [("Content-Type", contentType)], content)

template resp*(content: JsonNode): void =
  ## Serializes ``content`` as the response, sets ``Http200`` as status code
  ## and "application/json" Content-Type.
  resp($content, contentType="application/json;charset=utf-8")

template resp*(code: HttpCode, content: string,
               contentType = "text/html;charset=utf-8"): void =
  ## Sets ``content`` as the response; ``code`` as the status code
  ## and ``contentType`` as the Content-Type.
  resp(code, [("Content-Type", contentType)], content)

template resp*(code: HttpCode): void =
  ## Responds with the specified ``HttpCode``. This ensures that error handlers
  ## are called.
  let ret = (code, none[RawHeaders](), "")
  result = some[ResponseData](ret)

proc params*(req: Request): StringTableRef =
  result = newStringTable(modeCaseSensitive)

  let query = req.path.get("").parseUri().query

  try:
    for key, val in cgi.decodeData(query):
      result[key] = val
  except CgiError:
    discard

template parseContentDisposition(): typed =
  var hCount = 0
  while hCount < hValue.len()-1:
    var key = ""
    hCount += hValue.parseUntil(key, {';', '='}, hCount)
    if hValue[hCount] == '=':
      var value = hvalue.captureBetween('"', start = hCount)
      hCount += value.len+2
      inc(hCount) # Skip ;
      hCount += hValue.skipWhitespace(hCount)
      if key == "name": name = value
      newPart[0][key] = value
    else:
      inc(hCount)
      hCount += hValue.skipWhitespace(hCount)

proc parseMultiPart*(body: string, boundary: string): MultiData =
  result = initOrderedTable[string, tuple[fields: StringTableRef, body: string]]()
  var mboundary = "--" & boundary

  var i = 0
  var partsLeft = true
  while partsLeft:
    var firstBoundary = body.skip(mboundary, i)
    if firstBoundary == 0:
      raise newException(ValueError, "Expected boundary. Got: " & body.substr(i, i+25))
    i += firstBoundary
    i += body.skipWhitespace(i)

    # Headers
    var newPart: tuple[fields: StringTableRef, body: string] = ({:}.newStringTable, "")
    var name = ""
    while true:
      if body[i] == '\c':
        inc(i, 2) # Skip \c\L
        break
      var hName = ""
      i += body.parseUntil(hName, ':', i)
      if body[i] != ':':
        raise newException(ValueError, "Expected : in headers.")
      inc(i) # Skip :
      i += body.skipWhitespace(i)
      var hValue = ""
      i += body.parseUntil(hValue, {'\c', '\L'}, i)
      if toLowerAscii(hName) == "content-disposition":
        parseContentDisposition()
      newPart[0][hName] = hValue
      i += body.skip("\c\L", i) # Skip *one* \c\L

    # Parse body.
    while true:
      if body[i] == '\c' and body[i+1] == '\L' and
         body.skip(mboundary, i+2) != 0:
        if body.skip("--", i+2+mboundary.len) != 0:
          partsLeft = false
          break
        break
      else:
        newPart[1].add(body[i])
      inc(i)
    i += body.skipWhitespace(i)

    result.add(name, newPart)


proc parseMPFD(contentType: string, body: string): MultiData =
  var boundaryEqIndex = contentType.find("boundary=")+9
  var boundary = contentType.substr(boundaryEqIndex, contentType.len()-1)
  return parseMultiPart(body, boundary)

proc formData*(req: Request): MultiData =
  let contentType: string = req.headers.get(newHttpHeaders()).getOrDefault("Content-Type")
  if contentType.startsWith("multipart/form-data"):
    result = parseMPFD(contentType, req.body.get)
