defmodule Channel do
  def init(channel_name) do
    {:ok, userAgent} = Storage.start_link()
    loop(%ChannelState{
      users: userAgent,
      name: channel_name,
    })
  end

  @spec loop(ChannelState.t()) :: no_return
  def loop(state) do
    receive do
      {:post, message} ->
        Storage.foreach(state.users, &send(&1, {:post, state.name, message}))
      {:join, name, address} ->
        case Storage.put(state.users, name, address) do
          :ok -> post(self(), "#{name} has joined the channel")
          :not_ok -> IO.puts :stderr, "#{name} already in the channel"
        end
      {:leave, name} ->
        Storage.delete(state.users, name)
        post(self(), "#{name} has left the channel")
    end
    loop(state)
  end

  defp post(me, message), do: send(me, {:post, message})
end
