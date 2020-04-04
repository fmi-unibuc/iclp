defmodule Server do
  @moduledoc """
  A server maintaining the list of channels and users
  """

  @doc """
  """
  def init() do
    general = spawn_link(Channel, :init, [:general])
    {:ok, usersAgent} = Storage.start_link()
    {:ok, channelsAgent} = Storage.start_link()
    {:ok, userThreadsAgent} = Storage.start_link()
    Storage.put!(channelsAgent, :general, general)
    loop(%ServerState{
      me: self(),
      users: usersAgent,
      channels: channelsAgent,
      userThreads: userThreadsAgent
    })
  end

  @spec loop(ServerState.t()) :: any()
  defp loop(state) do
    receive do
      {:register, name, address} ->
        case Storage.put(state.users, name, address) do
          :ok ->
            {thread, userChannels} = ServerThread.init(state.channels, name, address)
            ref = Process.monitor(thread)
            Storage.put!(state.userThreads, ref, {thread, name, userChannels})
            send(address, {:registered, name, state.me, thread})
          :not_ok -> send(address, {:not_registered, name, state.me})
        end
      {:DOWN, ref, :process, _thread, _reason} ->
        case Storage.get(state.userThreads, ref) do
          nil -> IO.puts :stderr, "[server]: invalid_reference #{ref}; ignored"
          {_thread, name, userChannels} ->
            Storage.foreach(userChannels, &send(&1, {:leave, name}))
            Storage.delete(state.userThreads, ref)
            Storage.delete(state.users, name)
        end
      message -> IO.inspect message
    end
    loop(state)
  end
end
