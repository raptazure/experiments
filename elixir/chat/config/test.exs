use Mix.Config
config :chat, remote_supervisor: fn _recipient -> Chat.TaskSupervisor end
