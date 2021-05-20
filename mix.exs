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
      {:libcluster, "~> 3.3"},
      {:telemetry_metrics_prometheus, "~> 1.0"},
      {:telemetry_poller, "~> 0.5.1"},
      {:opentelemetry_api, "~> 0.6"},
      {:opentelemetry, "~> 0.6"},
      :opentelemetry_zipkin
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
