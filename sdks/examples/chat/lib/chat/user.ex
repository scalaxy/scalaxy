defmodule Chat.User do
  @moduledoc "A chat user handle. Persisted as a `:ChatUser` node in Scalaxy."
  use Ash.Resource,
    domain: Chat.Domain,
    data_layer: Chat.ScalaxyDataLayer

  attributes do
    attribute :id, :string, primary_key?: true, allow_nil?: false, generated?: false, writable?: true
    attribute :handle, :string, allow_nil?: false, public?: true
    attribute :color, :string, default: "#6366f1", public?: true
    create_timestamp :inserted_at, public?: true
  end

  actions do
    defaults [:read]

    create :create do
      accept [:id, :handle, :color]
      primary? true
    end
  end

  code_interface do
    define :create, action: :create
    define :read, action: :read
  end
end
