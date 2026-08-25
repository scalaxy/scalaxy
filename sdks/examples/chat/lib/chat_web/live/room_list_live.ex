defmodule ChatWeb.RoomListLive do
  @moduledoc "Lobby: list rooms and create new ones."
  use ChatWeb, :live_view

  @impl true
  def mount(_params, session, socket) do
    if connected?(socket), do: Phoenix.PubSub.subscribe(Chat.PubSub, "rooms")

    {:ok,
     assign(socket,
       rooms: Chat.GraphStore.list_rooms(),
       handle: session["user_handle"],
       page_title: "Scalaxy Chat - Rooms"
     )}
  end

  @impl true
  def handle_event("create", %{"room" => %{"name" => name, "topic" => topic}}, socket) do
    name = String.trim(name)

    with false <- name == "",
         {:ok, _room} <- Chat.GraphStore.create_room(%{name: name, topic: String.trim(topic)}) do
      Phoenix.PubSub.broadcast(Chat.PubSub, "rooms", :refresh)
      {:noreply, assign(socket, rooms: Chat.GraphStore.list_rooms())}
    else
      _ -> {:noreply, put_flash(socket, :error, "Room name is required.")}
    end
  end

  @impl true
  def handle_info(:refresh, socket) do
    {:noreply, assign(socket, rooms: Chat.GraphStore.list_rooms())}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div class="mx-auto max-w-3xl px-6 py-12">
      <header class="mb-10 text-center">
        <h1 class="text-4xl font-bold tracking-tight text-zinc-50">
          Scalaxy<span class="text-indigo-400">Chat</span>
        </h1>
        <p class="mt-2 text-sm text-zinc-400">
          Multi-room, multi-user chat. Every room and message is stored in Scalaxy via Cypher.
        </p>
      </header>

      <section class="mb-10 rounded-xl border border-zinc-800 bg-zinc-900/60 p-6">
        <h2 class="mb-4 text-xs font-semibold uppercase tracking-wider text-zinc-500">Your identity</h2>
        <form action="/session" method="post" class="flex gap-3">
          <input type="hidden" name="_csrf_token" value={Phoenix.Controller.get_csrf_token()} />
          <input name="handle" placeholder="Pick a handle..." required maxlength="32"
                 value={@handle}
                 class="flex-1 rounded-lg border border-zinc-700 bg-zinc-800 px-4 py-2 text-sm outline-none focus:border-indigo-500" />
          <button class="rounded-lg bg-indigo-600 px-5 py-2 text-sm font-medium hover:bg-indigo-500">
            Save handle
          </button>
        </form>
        <%= if @handle do %>
          <p class="mt-2 text-xs text-emerald-400">Joined as <strong><%= @handle %></strong></p>
        <% end %>
      </section>

      <section>
        <div class="mb-4 flex items-center justify-between">
          <h2 class="text-xs font-semibold uppercase tracking-wider text-zinc-500">Rooms</h2>
        </div>
        <form phx-submit="create" class="mb-6 flex gap-3">
          <input name="room[name]" placeholder="new-room-name" required maxlength="64"
                 class="w-56 rounded-lg border border-zinc-700 bg-zinc-800 px-4 py-2 text-sm outline-none focus:border-indigo-500" />
          <input name="room[topic]" placeholder="Topic (optional)" maxlength="140"
                 class="flex-1 rounded-lg border border-zinc-700 bg-zinc-800 px-4 py-2 text-sm outline-none focus:border-indigo-500" />
          <button class="rounded-lg border border-indigo-500 px-5 py-2 text-sm font-medium text-indigo-300 hover:bg-indigo-950">
            + Create room
          </button>
        </form>

        <ul class="divide-y divide-zinc-800 rounded-xl border border-zinc-800 bg-zinc-900/40">
          <li :for={room <- @rooms}>
            <a href={"/rooms/#{room.id}"} class="flex items-center justify-between px-5 py-4 hover:bg-zinc-900">
              <span class="font-semibold text-indigo-300">#<%= room.name %></span>
              <span class="text-sm text-zinc-500"><%= room.topic %></span>
            </a>
          </li>
          <li :if={@rooms == []} class="px-5 py-8 text-center text-sm text-zinc-500">
            No rooms yet -- create the first one above.
          </li>
        </ul>
      </section>
    </div>
    """
  end
end
