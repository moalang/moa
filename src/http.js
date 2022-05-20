module.exports = () => {
  const _version = '0.0.2'
  const h = require('http')
  const fs = require('fs')
  const path = require('path')
  const types = {
    html: 'text/html; charset=utf-8',
    js: 'application/javascript',
    css: 'text/css',
    png: 'image/png',
    jpg: 'image/jpeg',
    gif: 'image/gif',
    webp: 'image/webp',
    ico: 'image/x-icon',
    svg: 'image/svg+xml; charset=utf-8'
  }
  const isHotLoading = process.env.DEV
  let onHotLoading = () => {}
  const _listen = (target, matcher) => {
    const gets = {}
    const posts = {}
    let dir = null
    matcher({
      _get: (s, f) => gets[s] = f,
      _post: (s, f) => posts[s] = f,
      _mount: d => dir = d
    })
    if (isHotLoading) {
      fs.watch('./', {recursive: true}, () => setTimeout(onHotLoading, 10))
    }
    __p(now()._string, `listen ${target}`)

    const server = h.createServer(async (request, response) => {
      if (isHotLoading && request.url === '/__moa_ping') {
        onHotLoading = () => response.end()
        return
      }
      let statusCode = 200
      let ctype = 'text/plain'
      let body = ''
      const time = now()
      const respond = (s, t, b) => { statusCode = s; ctype = t; body = b }
      const exposure = {
        _json: (obj, status) => respond(200, 'application/json; charset=utf-8', typeof obj === 'string' ? obj : JSON.stringify(obj))
      }
      try {
        if (request.method === 'POST' && request.url in posts) {
          await posts[request.url](exposure)
        } else if (request.method === 'GET') {
          if (request.url in gets) {
            await gets[request.url](exposure)
          } else {
            const normalizedPath = request.url.endsWith('/') ? request.url + 'index.html' : request.url
            const data = await new Promise((resolve, reject) => fs.readFile(path.join(dir, normalizedPath), (err, data) => err ? reject(err) : resolve(data)))
            if (isHotLoading && normalizedPath.endsWith('.html')) {
              const script = '\n\n<script>var s = document.createElement("script"); s.src = "/__moa_ping"; s.onload = () => location.reload(); window.onload = () => document.body.appendChild(s)</script>'
              respond(200, types.html, data + script)
            } else {
              respond(200, types[path.extname(normalizedPath).slice(1)] || 'text/plain', data)
            }
          }
        } else {
          respond(404, 'text/plain', '')
        }
        __p(now()._string, statusCode, time._elapsed(), request.url)
      } catch (e) {
        if (e.code === 'ENOENT') {
          respond(404, 'text/plain', '')
        } else {
          respond(500, 'text/plain', e.message)
        }
        __p(now()._string, statusCode, time._elapsed(), request.url, e.stack)
      } finally {
        response.writeHead(statusCode, {'Content-Type': ctype})
        response.end(body)
      }
    })
    server.listen(...target.split(':').reverse())
  }
  return { _version, _listen }
}
