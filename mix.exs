defmodule Andy.MixProject do
  use Mix.Project

  def project do
    [
      app: :andy,
      version: "0.1.0",
      elixir: "~> 1.11",
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      releases: releases()
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      mod: {:andy_app, []},
      extra_applications: [:mnesia]
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:redis, git: "https://github.com/niamtokik/redis", ref: "2d2790f"},
      {:libcluster, "~> 3.3"}
      # {:dep_from_hexpm, "~> 0.3.0"},
      # {:dep_from_git, git: "https://github.com/elixir-lang/my_dep.git", tag: "0.1.0"}
    ]
  end

  defp releases() do
    [
      app: [
        include_executables_for: [:unix]
      ]
    ]
  end
end
