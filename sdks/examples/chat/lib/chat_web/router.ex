defmodule ChatWeb.Router do
  use ChatWeb, :router

  pipeline :browser do
    plug :accepts, ["html"]
    plug :fetch_session
    plug :fetch_live_flash
    plug :put_root_layout, html: {ChatWeb.Layouts, :root}
    plug :protect_from_forgery
    plug :put_secure_browser_headers
  end

  scope "/", ChatWeb do
    pipe_through :browser

    live "/", RoomListLive
    live "/rooms/:id", ChatRoomLive
    post "/session", SessionController, :create
  end

  # Enable LiveDashboard in development
  if Application.compile_env(:chat, :dev_routes) do
    import Phoenix.LiveDashboard.Router

    scope "/dev" do
      pipe_through :browser
      live_dashboard "/dashboard", metrics: ChatWeb.Telemetry
    end
  end
end
