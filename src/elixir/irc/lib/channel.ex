defmodule Channel do
  def init(channel_name) do
    loop(%ChannelState{
      users: spawn_link(Storage, :init, []),
      name: channel_name,
    })
  end

  def loop(state) do
    receive do
      {:post, message} ->
        send(state.users, {:foreach, &send(&1, {:post, state.name, message})})
      {:join, name, address} ->
        me = self()
        send(state.users, {:put, name, address, &handleJoin(me, name, &1)})
      {:leave, name} ->
        send(state.users, {:delete, name})
        post(self(), "#{name} has left the channel")
    end
    loop(state)
  end

  defp post(me, message), do: send(me, {:post, message})

  defp handleJoin(me, name, :ok), do:
    post(me, "#{name} has joined the channel")
  defp handleJoin(_me, name, :not_ok), do:
    IO.puts :stderr, "#{name} already in the channel"
end
