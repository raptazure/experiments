defmodule Chat do
  def receive_message(message) do
    IO.puts(message)
  end

  def receive_message_for_cxs(message, from) do
    IO.puts(message)
    send_message(from, "我从不卖菜 我是真的菜")
  end

  def send_message(:cxs@localhost, message) do
    spawn_task(__MODULE__, :receive_message_for_cxs, :cxs@localhost, [
      message,
      Node.self()
    ])
  end

  def send_message(recipient, message) do
    spawn_task(__MODULE__, :receive_message, recipient, [message])
  end

  def spawn_task(module, fun, recipient, args) do
    recipient
    |> remote_supervisor()
    |> Task.Supervisor.async(module, fun, args)
    |> Task.await()
  end

  defp remote_supervisor(recipient) do
    {Chat.TaskSupervisor, recipient}
  end

  # defp remote_supervisor(recipient) do
  #   Application.get_env(:chat, :remote_supervisor).(recipient)
  # end
end
