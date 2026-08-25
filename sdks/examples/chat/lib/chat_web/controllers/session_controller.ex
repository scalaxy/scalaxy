defmodule ChatWeb.SessionController do
  @moduledoc "Stores the chosen display handle in the browser session."
  use ChatWeb, :controller

  def create(conn, %{"handle" => handle} = params) when byte_size(handle) in 1..32 do
    conn
    |> put_session(:user_handle, String.trim(handle))
    |> put_session(:user_color, Map.get(params, "color", "#6366f1"))
    |> redirect(to: "/")
  end

  def create(conn, _params), do: redirect(conn, to: "/")
end
