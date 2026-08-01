# radium-crashreport Worker

Receives crash reports POSTed by the Radium crash reporter (`crashreporter/crashreporter.cpp`,
`send_crash_message_to_server`) and emails them to `k.s.matheussen@gmail.com` using the
Cloudflare Email Service `send_email` binding.

## Prerequisites (one-time, in the Cloudflare dashboard)

- `radium.dog` is onboarded to Email Service / Email Routing and the destination address
  `k.s.matheussen@gmail.com` is verified (add it under Email Routing > Destination Addresses
  and click the verification link Cloudflare emails to it).
- Install Wrangler: `npm install -g wrangler` (or use `npx wrangler`).

## Deploy

```bash
npx wrangler deploy
npx wrangler secret put CRASHREPORT_SECRET
```

`CRASHREPORT_SECRET` must be set to the same token that is hardcoded as
`CRASHREPORT_SECRET` in `crashreporter/crashreporter.cpp`.

## Test locally

```bash
npx wrangler dev
curl -X POST http://localhost:8787 \
  -H "X-Radium-Secret: <secret>" \
  --data 'data=Radium%20version%201.0%0Acrashed%20here\n<br>1.%20Start%20Radium'
```

A request without the secret header gets `401`. A request without `\n<br>` treats the whole
body as the encoded `data` field. The email send is retried up to 3 times with a short
backoff (the destination was previously the flaky `notam02.no` mail server; Gmail has been
reliable). If it still fails, the worker returns `500` with a JSON body containing the error
code (e.g. `E_DELIVERY_FAILED`), which also appears in the Cloudflare worker logs.

Each request is logged with a short diagnostic line (report length, control-character and
surrogate counts) and a 2000-character preview of the decoded report. Remove
`logReportDiagnostics()` in `src/index.ts` if you don't want report content in the logs.
