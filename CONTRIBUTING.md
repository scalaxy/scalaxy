# Contributing to Scalaxy

Thanks for your interest in contributing! Scalaxy is an independent,
open-source project, and contributions of code, documentation, tests,
bug reports, and ideas are all welcome.

## Getting started

1. **Fork** the repository on GitHub.
2. **Clone** your fork and add the upstream remote:

   ```sh
   git clone git@github.com:<you>/scalaxy.git
   cd scalaxy
   git remote add upstream https://github.com/scalaxy/scalaxy.git
   ```

3. **Run the tests** before changing anything:

   ```sh
   make test
   ```

## Development workflow

1. Create a topic branch from `master`:

   ```sh
   git checkout -b feature/my-change
   ```

2. Make a **focused change** and add tests alongside any behavior change.
3. Keep the suite green: `make test` must pass locally (8,654 checks).
4. Push your branch and open a pull request with a clear description of
   the change and the tests that cover it.

## Style guidelines

- Portable ANSI Common Lisp where possible; keep SBCL-specific code behind
  `#+sbcl` and out of the core modules.
- The JSON / HTTP / TCP layers are intentionally **dependency-free** — keep
  them that way.
- Every public function should have a docstring.
- Match the existing formatting (2-space indentation, `defstruct` accessors,
  `+constants+` naming).
- Run `node --check web/assets/app.js` if you touch the web console JS.

## Testing

```sh
make test                  # full suite (13 groups, 8,654 checks)
make build                 # compile the systems with ASDF
```

The suite uses real sockets on ephemeral ports and real threads, so it
exercises actual TCP/HTTP traffic, replication, failover, and log replay.

## Pull request checklist

- [ ] Tests pass locally (`make test`)
- [ ] New behavior is covered by tests
- [ ] Docstrings/documentation updated
- [ ] No unrelated changes in the PR

## Reporting issues

Please open a GitHub issue with a clear title, the version you are using
(`scalaxy --id` / the footer of the web console), and steps to reproduce.

## Code of conduct

By participating you agree to abide by the [Code of Conduct](CODE_OF_CONDUCT.md).
