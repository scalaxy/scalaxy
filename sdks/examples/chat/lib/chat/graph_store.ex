defmodule Chat.GraphStore do
  @moduledoc """
  Thin service layer over the Ash domain. Everything below persists to
  Scalaxy as Cypher through `Chat.ScalaxyDataLayer`, and reads fan back
  out as Ash resource structs.
  """

  require Ash.Query
  require Ash.Expr
  import Ash.Expr

  alias Chat.{Message, Room, User}

  ## rooms

  def list_rooms do
    case Ash.read(Room, authorize?: false) do
      {:ok, rooms} -> Enum.sort_by(rooms, & &1.name)
      {:error, _} -> []
    end
  end

  def get_room!(id) do
    Room
    |> Ash.Query.filter(expr(id == ^id))
    |> Ash.read_one!(authorize?: false)
  end

  def create_room(attrs) do
    Room
    |> Ash.Changeset.for_create(:create, Map.put_new(attrs, :id, uuid7()))
    |> Ash.create(authorize?: false)
  end

  ## messages

  def messages_for(room_id, limit \\ 100) do
    {:ok, messages} =
      Message
      |> Ash.Query.filter(expr(room_id == ^room_id))
      |> Ash.Query.sort(inserted_at: :asc)
      |> Ash.Query.limit(limit)
      |> Ash.read(authorize?: false)

    messages
  end

  def post_message(attrs) do
    Message
    |> Ash.Changeset.for_create(:create, Map.put_new(attrs, :id, uuid7()))
    |> Ash.create(authorize?: false)
  end

  ## users

  def ensure_user(handle, color) do
    case User
         |> Ash.Query.filter(expr(handle == ^handle))
         |> Ash.read_one(authorize?: false) do
      {:ok, %Chat.User{} = user} ->
        user

      _ ->
        User
        |> Ash.Changeset.for_create(:create, %{id: uuid7(), handle: handle, color: color})
        |> Ash.create(authorize?: false)
        |> case do
          {:ok, user} -> user
          _ -> nil
        end
    end
  end

  @doc "UUIDv7: time-ordered, so Cypher string sorts are chronological."
  def uuid7 do
    import Bitwise

    ms = System.system_time(:millisecond)
    <<r::unsigned-size(80)>> = :crypto.strong_rand_bytes(10)

    rand_a = r >>> 68 &&& 0xFFF
    rand_b = r &&& 0x3FFF_FFFF_FFFF_FFFF

    full =
      (ms <<< 80) +
        (7 <<< 76) +
        (rand_a <<< 64) +
        (2 <<< 62) +
        rand_b

    hex = hex128(full)

    <<a::binary-size(8), b::binary-size(4), c::binary-size(4), d::binary-size(4),
      e::binary-size(12)>> = hex

    "#{a}-#{b}-#{c}-#{d}-#{e}"
  end

  defp hex128(n) do
    n
    |> Integer.to_string(16)
    |> String.downcase()
    |> String.pad_leading(32, "0")
  end

end
