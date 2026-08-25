defmodule Escalaxy.Application do
  @moduledoc false
  use Application

  @impl true
  def start(_type, _args) do
    children = [
      {Finch,
       name: Escalaxy.Finch,
       pools: %{
         default: [size: 20, count: 4]
       }}
    ]

    Supervisor.start_link(children, strategy: :one_for_one, name: Escalaxy.Supervisor)
  end
end
