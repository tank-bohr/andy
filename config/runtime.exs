import Config

log_level =
  "LOG_LEVEL"
  |> System.fetch_env!()
  |> String.to_existing_atom()

config :logger, level: log_level

config :andy, db_backend: :andy_mnesia_backend
