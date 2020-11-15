use Mix.Config
config :chat, remote_supervisor: fn recipient -> {Chat.TaskSupervisor, recipient} end
