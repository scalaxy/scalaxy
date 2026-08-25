defmodule ChatWeb.Presence do
  @moduledoc "Tracks who is currently in each room."
  use Phoenix.Presence,
    otp_app: :chat,
    pubsub_server: Chat.PubSub
end
