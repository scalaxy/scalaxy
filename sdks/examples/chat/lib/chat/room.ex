defmodule Chat.Room do
  @moduledoc "A chat room. Persisted as a `:Room` node in Scalaxy."
  use Ash.Resource,
    domain: Chat.Domain,
    data_layer: Chat.ScalaxyDataLayer

  attributes do
    attribute :id, :string, primary_key?: true, allow_nil?: false, generated?: false, writable?: true
    attribute :name, :string, allow_nil?: false, public?: true
    attribute :topic, :string, default: "", public?: true
    create_timestamp :inserted_at, public?: true
    update_timestamp :updated_at, public?: true
  end

  actions do
    defaults [:read, :destroy]

    create :create do
      accept [:id, :name, :topic]
      primary? true
    end

    update :update do
      accept [:id, :name, :topic]
      primary? true
    end
  end

  code_interface do
    define :create, action: :create
    define :read, action: :read
    define :destroy, action: :destroy
  end

  identities do
    identity :unique_name, [:name], pre_check_with: Chat.ScalaxyDataLayer
  end
end
