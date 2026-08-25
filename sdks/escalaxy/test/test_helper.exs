ExUnit.start()

# Integration tests need a live cluster; excluded unless explicitly included:
#   SCALAXY_URL=http://localhost:8080 mix test --include integration
ExUnit.configure(exclude: :integration)
