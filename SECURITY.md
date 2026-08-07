# Security Policy

## Supported versions

Security fixes are applied to the latest release. Older releases receive
fixes on a best-effort basis.

| Version | Supported          |
|---------|--------------------|
| 1.6.x   | :white_check_mark: |
| < 1.6   | :x:                |

## Reporting a vulnerability

**Please do not open a public issue for security problems.**

Report vulnerabilities privately through GitHub's **Security Advisory**
feature:

1. Open the repository page on GitHub.
2. Go to **Security** → **Report a vulnerability** (or **Advisories** →
   **New draft security advisory**).
3. Include:
   - the affected version,
   - a description of the issue,
   - steps to reproduce,
   - and, if possible, a minimal proof of concept.

You can expect an acknowledgment within a few business days and a detailed
response about next steps.

## Security-relevant design notes

- The wire protocol is length-prefixed and opcode-validated; decoders never
  allocate beyond the declared frame length.
- The web console binds its HTTP server to `0.0.0.0:8080` by default only
  when configured; for untrusted networks, put the console behind an
  authenticated reverse proxy or bind it to a private interface.
- The data plane has no built-in authentication; deploy it inside a private
  network or a service mesh when using multi-node clusters.
