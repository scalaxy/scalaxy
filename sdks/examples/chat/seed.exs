
alias Chat.GraphStore

rooms =
  case GraphStore.create_room(%{name: "general", topic: "Welcome to Scalaxy Chat"}) do
    {:ok, room} -> [room]
    {:error, _} -> GraphStore.list_rooms() |> Enum.filter(&(&1.name == "general"))
  end

case GraphStore.create_room(%{name: "random", topic: "Off-topic chatter"}) do
  {:ok, r} -> :ok
  _ -> :ok
end

general = hd(rooms)
Chat.GraphStore.post_message(%{
  room_id: general.id,
  author: "scalaxy",
  body: "Welcome! Every room, user and message here is stored in Scalaxy as Cypher graph nodes."
})

IO.puts("seeded. general room id: " <> general.id)
System.stop(0)
