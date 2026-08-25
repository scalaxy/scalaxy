defmodule Escalaxy.MixProject do
  use Mix.Project

  @version "0.1.0"
  @source_url "https://github.com/scalaxy/escalaxy-elixir"

  def project do
    [
      app: :escalaxy,
      version: @version,
      elixir: "~> 1.15",
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      docs: docs(),
      package: package(),
      description: description(),
    ]
  end

  def cli do
    [preferred_envs: [docs: :docs]]
  end

  def application do
    [extra_applications: [:logger], mod: {Escalaxy.Application, []}]
  end

  defp deps do
    [
      {:finch, "~> 0.19"},
      {:jason, "~> 1.4"},
      {:telemetry, "~> 1.2"},
      {:ex_doc, "~> 0.34", only: :dev, runtime: false}
    ]
  end

  defp description do
    "Official Elixir client SDK for Scalaxy - the S3-backed distributed graph " <>
      "database. Query with Cypher over HTTP from any Elixir or Phoenix application."
  end

  defp docs do
    [
      main: "Escalaxy",
      source_url: @source_url,
      source_ref: "v#{@version}",
      extras: ["README.md"]
    ]
  end

  defp package do
    [
      name: :escalaxy,
      licenses: ["Apache-2.0"],
      links: %{"GitHub" => @source_url},
      files: ~w(lib mix.exs README.md LICENSE)
    ]
  end
end
