defmodule Message do
  def listen do
    receive do
      {:ok, "hello"} -> IO.puts("World")
    end

    listen()
  end
end

pid = spawn(Message, :listen, [])
send(pid, {:ok, "hello"})

defmodule Link do
  def explode, do: exit(:kaboom)

  def run do
    Process.flag(:trap_exit, true)
    spawn_link(Link, :explode, [])

    receive do
      {:EXIT, _from_pid, reason} -> IO.puts("Exit reason: #{reason}")
    end
  end
end

defmodule Monitor do
  def explode, do: exit(:kaboom)

  def run do
    spawn_monitor(Monitor, :explode, [])

    receive do
      {:DOWN, _ref, :process, _from_pid, reason} -> IO.puts("Exit reason: #{reason}")
    end
  end
end

{:ok, agent} = Agent.start_link(fn -> [1, 2, 3] end)
Agent.update(agent, fn state -> state ++ [4, 5] end)
# [1, 2, 3, 4, 5]
Agent.get(agent, & &1)

# Agent.start_link(fn -> [1, 2, 3] end, name: Numbers)
# Agent.get(Numbers, &(&1))

defmodule Tasks do
  def double(x) do
    :timer.sleep(2000)
    x * 2
  end
end

# task = Task.async(Tasks, :double, [2000])
# Task.await(task)
