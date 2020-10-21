defmodule SimpleQueue.Application do
  # See https://hexdocs.pm/elixir/Application.html
  # for more information on OTP Applications
  @moduledoc false

  use Application

  @spec start(any, any) :: {:error, any} | {:ok, pid}
  def start(_type, _args) do
    children = [
      # Starts a worker by calling: SimpleQueue.Worker.start_link(arg)
      # {SimpleQueue.Worker, arg}
      {SimpleQueue, [1, 2, 3]}
    ]

    # See https://hexdocs.pm/elixir/Supervisor.html
    # for other strategies and supported options
    opts = [strategy: :one_for_one, name: SimpleQueue.Supervisor]
    Supervisor.start_link(children, opts)
  end

  def child_spec(opts) do
    %{
      id: SimpleQueue,
      start: {__MODULE__, :start_link, [opts]},
      shutdown: 5_000,
      restart: :permanent,
      type: :worker
    }
  end

  # Dynamic Supervisor
  # options = [
  #   name: SimpleQueue.Supervisor,
  #   strategy: :one_for_one
  # ]

  # DynamicSupervisor.start_link(options)
  # {:ok, _pid} = DynamicSupervisor.start_child(SimpleQueue.Supervisor, SimpleQueue)

  # Task Supervisor
  # children = [
  #   {Task.Supervisor, name: ExampleApp.TaskSupervisor, restart: :transient}
  # ]

  # {:ok, pid} = Supervisor.start_link(children, strategy: :one_for_one)

  # use the start_child/2 function to create a supervised task:
  # {:ok, pid} = Task.Supervisor.start_child(ExampleApp.TaskSupervisor, fn -> background_work end)
end
