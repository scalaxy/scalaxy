defmodule Chat.Message do
  @moduledoc "A chat message. Persisted as a `:Message` node in Scalaxy."
  use Ash.Resource,
    domain: Chat.Domain,
    data_layer: Chat.ScalaxyDataLayer

  attributes do
    attribute :id, :string, primary_key?: true, allow_nil?: false, generated?: false, writable?: true
    attribute :room_id, :string, allow_nil?: false, public?: true
    attribute :author, :string, allow_nil?: false, public?: true
    attribute :body, :string, allow_nil?: false, public?: true
    create_timestamp :inserted_at, public?: true
  end

  actions do
    defaults [:read]

    create :create do
      accept [:id, :room_id, :author, :body]
      primary? true
    end

    destroy :destroy do
      primary? true
    end
  end

  code_interface do
    define :create, action: :create
    define :read, action: :read
    define :destroy, action: :destroy
  end
end
