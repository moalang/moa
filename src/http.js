module.exports = () => {
  const _version = '0.0.2'
  const h = require('http')
  const fs = require('fs')
  const path = require('path')
  const crypto = require('crypto')
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
  const etagLayer = (request, o) => {
    if (request.method !== 'GET' || o.status !== 200) {
      return o
    }
    const etag = crypto.createHash('md5').update(o.body, 'binary').digest('hex')
    if (request.headers['if-none-match'] !== etag) {
      o.headers['Etag'] = etag
      return o
    }
    return Object.assign(o, {status: 304, body: '', headers: {Etag: etag}})
  }
  const handle = async (request, gets, posts, dir) => {
    const wrap = (status, ctype, body, headers, error) => ({status, headers: { 'Content-Type': ctype }, body: body || '', error })
    const exposure = {
      _json: (obj, status) => wrap(200, 'application/json; charset=utf-8', JSON.stringify(obj))
    }
    try {
      if (request.method === 'POST' && request.url in posts) {
        return await posts[request.url](exposure)
      } else if (request.method === 'GET') {
        if (request.url in gets) {
          return await gets[request.url](exposure)
        } else if (dir) {
          const normalizedPath = request.url.endsWith('/') ? request.url + 'index.html' : request.url
          const data = await new Promise((resolve, reject) => fs.readFile(path.join(dir, normalizedPath), (err, data) => err ? reject(err) : resolve(data)))
          if (isHotLoading && normalizedPath.endsWith('.html')) {
            const script = '\n\n<script>var s = document.createElement("script"); s.src = "/__moa_ping"; s.onload = () => location.reload(); window.onload = () => document.body.appendChild(s)</script>'
            return wrap(200, types.html, data + script)
          } else {
            return wrap(200, types[path.extname(normalizedPath).slice(1)] || 'text/plain', data)
          }
        }
      } else {
        return wrap(404, 'text/plain')
      }
    } catch (e) {
      if (e.code === 'ENOENT') {
        return wrap(404, 'text/plain')
      } else {
        return wrap(500, 'text/plain', e.message, {}, e)
      }
    }
  }
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
      const time = now()
      const {status, headers, body, error} = etagLayer(request, await handle(request, gets, posts, dir))
      response.writeHead(status, headers)
      response.end(body)
      if (error) {
        __p(now()._string, status, time._elapsed(), request.url, error.stack)
      } else {
        __p(now()._string, status, time._elapsed(), request.url)
      }
    })
    server.listen(...target.split(':').reverse())
  }
  return { _version, _listen }
}
