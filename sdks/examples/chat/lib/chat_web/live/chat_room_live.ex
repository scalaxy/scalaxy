defmodule ChatWeb.ChatRoomLive do
  @moduledoc """
  A single chat room: message stream, send form and live presence of
  everyone currently in the room.
  """
  use ChatWeb, :live_view

  alias Chat.GraphStore

  @impl true
  def mount(%{"id" => room_id}, session, socket) do
    room = GraphStore.get_room!(room_id)

    handle = session["user_handle"] || "guest-" <> random_suffix()
    color = session["user_color"] || "#6366f1"

    if connected?(socket) do
      Phoenix.PubSub.subscribe(Chat.PubSub, topic(room_id))
      {:ok, _} = ChatWeb.Presence.track(self(), presence_topic(room_id), handle, %{color: color})
    end

    {:ok,
     assign(socket,
       room: room,
       room_id: room_id,
       handle: handle,
       color: color,
       messages: GraphStore.messages_for(room_id),
       users: presence_users(room_id, handle),
       page_title: "##{room.name} - Scalaxy Chat"
     )}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div class="flex h-screen flex-col bg-zinc-950 text-zinc-100">
      <.header_bar room={@room} />
      <div class="flex flex-1 overflow-hidden">
        <.sidebar users={@users} count={map_size(@users)} handle={@handle} />
        <div class="flex flex-1 flex-col">
          <.message_stream messages={@messages} handle={@handle} color={@color} />
          <.composer />
        </div>
      </div>
    </div>
    """
  end

  @impl true
  def handle_event("send", %{"body" => body}, socket)
      when byte_size(body) > 0 and byte_size(body) <= 2000 do
    # Persist first: the message is durable in Scalaxy before it is
    # fanned out to the room over PubSub.
    case GraphStore.post_message(%{
           room_id: socket.assigns.room_id,
           author: socket.assigns.handle,
           body: String.trim(body)
         }) do
      {:ok, msg} ->
        Phoenix.PubSub.broadcast(Chat.PubSub, topic(socket.assigns.room_id), {:new_message, msg})
        {:noreply, add_message(socket, msg)}

      {:error, _} ->
        {:noreply, put_flash(socket, :error, "Could not save message.")}
    end
  end

  def handle_event("send", _params, socket), do: {:noreply, socket}

  defp add_message(socket, msg) do
    case Enum.any?(socket.assigns.messages, &(&1.id == msg.id)) do
      true -> socket
      false -> assign(socket, messages: socket.assigns.messages ++ [msg])
    end
  end

  @impl true
  def handle_info({:new_message, msg}, socket) do
    {:noreply, add_message(socket, msg)}
  end

  @impl true
  def handle_info(%{event: "presence_diff"}, socket) do
    {:noreply,
     assign(socket, users: presence_users(socket.assigns.room_id, socket.assigns.handle))}
  end



  ## components

  attr :room, :map, required: true
  defp header_bar(assigns) do
    ~H"""
    <header class="flex items-center justify-between border-b border-zinc-800 bg-zinc-900 px-6 py-3">
      <div class="flex items-center gap-3">
        <a href="/" class="text-indigo-400 hover:text-indigo-300" aria-label="Back to lobby">&#8592;</a>
        <h1 class="text-lg font-semibold">#<%= @room.name %></h1>
        <span class="rounded-full bg-zinc-800 px-2 py-0.5 text-xs text-zinc-400"><%= @room.topic %></span>
      </div>
      <span class="text-xs uppercase tracking-wider text-zinc-500">stored in Scalaxy &#183; Cypher</span>
    </header>
    """
  end

  attr :users, :map, required: true
  attr :count, :integer, required: true
  attr :handle, :string, required: true
  defp sidebar(assigns) do
    ~H"""
    <aside class="hidden w-60 shrink-0 border-r border-zinc-800 bg-zinc-900/50 p-4 md:block">
      <h2 class="mb-3 text-xs font-semibold uppercase tracking-wider text-zinc-500">
        Online - <%= @count %>
      </h2>
      <ul class="space-y-2">
        <li :for={{name, metas} <- @users} class="flex items-center gap-2">
          <% meta = List.first(metas[:metas] || []) || %{} %>
          <span class="h-2 w-2 rounded-full" style={"background: #{meta[:color]}"}></span>
          <span class={if name == @handle, do: "font-semibold"}><%= name %></span>
        </li>
      </ul>
    </aside>
    """
  end

  attr :messages, :list, required: true
  attr :handle, :string, required: true
  attr :color, :string, required: true
  defp message_stream(assigns) do
    ~H"""
    <div id="messages" class="flex-1 space-y-3 overflow-y-auto p-6">
      <article :for={msg <- @messages} id={"msg-#{msg.id}"} class="max-w-2xl">
        <p class="text-sm">
          <span class="font-semibold" style={"color: #{@color}"}><%= msg.author %></span>
          <span class="ml-2 text-xs text-zinc-500"><%= format_time(msg.inserted_at) %></span>
        </p>
        <p class="mt-1 whitespace-pre-wrap break-words rounded-lg bg-zinc-900 px-3 py-2"><%= msg.body %></p>
      </article>
      <p :if={@messages == []} class="text-sm text-zinc-500">No messages yet. Say hello!</p>
    </div>
    """
  end

  defp composer(assigns) do
    ~H"""
    <form phx-submit="send" class="border-t border-zinc-800 bg-zinc-900 p-4">
      <div class="flex gap-3">
        <input name="body" autocomplete="off" placeholder="Message the room..."
               maxlength="2000" autofocus
               class="flex-1 rounded-lg border border-zinc-700 bg-zinc-800 px-4 py-2 text-sm outline-none focus:border-indigo-500" />
        <button type="submit"
                class="rounded-lg bg-indigo-600 px-5 py-2 text-sm font-medium hover:bg-indigo-500">
          Send
        </button>
      </div>
    </form>
    """
  end

  ## helpers

  defp topic(room_id), do: "room:" <> room_id
  defp presence_topic(room_id), do: "room:presence:" <> room_id

  defp presence_users(room_id, me) do
    ChatWeb.Presence.list(presence_topic(room_id))
    |> Map.new(fn {k, v} -> {k, v} end)
    |> Map.put_new(me, %{metas: [%{color: "#6366f1"}]})
  end

    defp format_time(%DateTime{} = dt), do: Calendar.strftime(dt, "%H:%M")
  defp format_time(other), do: to_string(other)

  defp random_suffix, do: :crypto.strong_rand_bytes(4) |> Base.encode16(case: :lower)
end
