defmodule Server do
  @moduledoc """
  A server maintaining the list of channels and users
  """

  @doc """
  """
  use GenServer

  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, :ok, opts)
  end

  def register(server, name) do
    GenServer.call(server, {:register, name})
  end

  @impl true
  @spec init(:ok) :: {:ok, ServerState.t()}
  def init(:ok) do
    {:ok, general} = Channel.start_link(:general, [])
    {:ok, usersAgent} = Storage.start_link()
    {:ok, channelsAgent} = Storage.start_link()
    {:ok, userThreadsAgent} = Storage.start_link()
    Storage.put!(channelsAgent, :general, general)
    {:ok, %ServerState{
      users: usersAgent,
      channels: channelsAgent,
      userThreads: userThreadsAgent
    }}
  end

  @impl true
  def handle_call({:register, name}, {address, _tag}, state) do
    case Storage.put(state.users, name, address) do
      :ok ->
        {:ok, localChannels} = Storage.start_link()
        initState =
          %ServerThreadState{
            userName: name,
            userAddress: address,
            globalChannels: state.channels,
            userChannels: localChannels
          }
        {:ok, thread} = ServerThread.start(initState)
        ref = Process.monitor(thread)
        Storage.put!(state.userThreads, ref, {thread, name, localChannels})
        {:reply, {:registered, thread}, state}
      :not_ok ->
        {:reply, :name_already_registered, state}
    end
  end

  @impl true
  def handle_info({:DOWN, ref, :process, _thread, _reason}, state) do
    case Storage.get(state.userThreads, ref) do
      nil -> IO.puts :stderr, "[server]: invalid_reference #{ref}; ignored"
      {_thread, name, userChannels} ->
        Storage.foreach(userChannels, &Channel.leave(&1, name))
        Storage.delete(state.userThreads, ref)
        Storage.delete(state.users, name)
    end
    {:noreply, state}
  end

  @impl true
  def handle_info(message, state) do
    IO.inspect message
    {:noreply, state}
  end
end
