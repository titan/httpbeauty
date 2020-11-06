import asyncdispatch, cgi, httpbeast, httpcore, json, options, strtabs, uri
import redis except `%`


type
  RawHeaders* = seq[tuple[key, val: string]]

  ResponseData* = tuple[
    code: HttpCode,
    headers: Option[RawHeaders],
    content: string,
  ]

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
