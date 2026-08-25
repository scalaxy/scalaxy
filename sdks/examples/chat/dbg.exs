import Ash.Expr
require Ash.Query
alias Chat.{Room}

try do
  Room |> Ash.Query.filter(name: "no-such-room-xyz") |> Ash.read!(authorize?: false)
  IO.puts("NO ERROR")
rescue
  e ->
    msg = Exception.format(:error, e)
    # find innermost cause line mentioning data layer or argument
    lines = String.split(msg, "\n")
    IO.puts(String.slice(Enum.join(lines, "\n"), 0, 400))
end
System.stop(0)
