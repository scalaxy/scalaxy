# escalaxy

Official Elixir client SDK for [Scalaxy](https://scalaxy.org) — the
S3-backed distributed graph database you query with Cypher.

## Installation

```elixir
def deps do
  [
    {:escalaxy, "~> 0.1.0"}
  ]
end
```

## Usage

```elixir
client = Escalaxy.Client.new(base_url: "http://localhost:8080", db: "mydb")

# Health check
:ok = Escalaxy.ping(client)

# Queries return {:ok, result} | {:error, exception}
{:ok, result} = Escalaxy.query(client, "CREATE (:Person {name: 'Ada'})")

result = Escalaxy.query!(client, "MATCH (p:Person) RETURN p.name")
Enum.to_list(result.records)
#=> [%{"p.name" => "Ada"}]

Escalaxy.scalar(result)  # single value of a 1x1 query

# Parameters
Escalaxy.query(client, "MATCH (p:Person {name: $name}) RETURN p",
  params: %{"name" => "Ada"})
```

### Safe Cypher building

```elixir
Escalaxy.Graph.create_node("Person", %{name: "O'Brien", age: 36})
#=> "CREATE (Person {age: 36, name: 'O\'Brien'})"

Escalaxy.Graph.delete_node("Person", :name, "Ada")
```

### Telemetry

Every query emits `[:escalaxy, :query, :stop]` with `%{duration: ...}`
and `%{status:, db:}` — attach a handler to collect metrics.

## Running tests

```bash
mix test                                    # unit tests only
SCALAXY_URL=http://localhost:8080 \
  mix test --include integration            # also run live-cluster tests
```

## Ash Framework integration

See `sdks/examples/chat` for a complete Phoenix application using the
`Chat.ScalaxyDataLayer` — an Ash data layer backed entirely by this SDK,
storing every resource as a labelled node in Scalaxy.
