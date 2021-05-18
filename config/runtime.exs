import Config

log_level =
  "LOG_LEVEL"
  |> System.fetch_env!()
  |> String.to_existing_atom()

config :logger, level: log_level

config :andy, db_backend: :andy_mnesia_backend

config :libcluster,
  topologies: [
    k8s: [
      strategy: Elixir.Cluster.Strategy.Kubernetes,
      config: [
        mode: :ip,
        kubernetes_node_basename: "andy",
        kubernetes_selector: "app=andy",
        kubernetes_service_name: "andy-service",
        kubernetes_ip_lookup_mode: :pods,
        kubernetes_namespace: "default"]]]

config :telemetry_poller, :default,
  name: :andy_telemetry_poller,
  measurements: [
    :memory
    :system_counts,
    :total_run_queue_lengths,
    {:process_info, [
        name:, :andy_connection,
        event:, [:andy_connection, :worker],
        keys:, [:memory, :message_queue_len]
    ]},
    {:andy_connection, :sessions_count, []}
  ]
