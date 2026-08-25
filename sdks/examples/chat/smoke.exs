{:ok, room} = Chat.GraphStore.create_room(%{name: "smoke-" <> Integer.to_string(System.system_time()), topic: "Smoke test room"})
IO.puts("ROOM CREATED id=" <> room.id)

{:ok, msg} = Chat.GraphStore.post_message(%{room_id: room.id, author: "ada", body: "first message!"})
IO.puts("MSG CREATED id=" <> msg.id)

msgs = Chat.GraphStore.messages_for(room.id)
IO.puts("MESSAGES FOR ROOM: " <> inspect(length(msgs)) <> " last=" <> inspect(hd(msgs).body))

rooms = Chat.GraphStore.list_rooms()
IO.puts("ROOMS: " <> inspect(length(rooms)))

user = Chat.GraphStore.ensure_user("grace", "#22d3ee")
IO.puts("USER: " <> user.handle)
System.stop(0)
