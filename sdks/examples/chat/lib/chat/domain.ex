defmodule Chat.Domain do
  @moduledoc "Ash domain for the chat example. All state lives in Scalaxy."
  use Ash.Domain,
    otp_app: :chat,
    validate_config_inclusion?: false

  resources do
    resource Chat.Room
    resource Chat.Message
    resource Chat.User
  end
end
