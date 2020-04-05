defmodule ServerThread do
  @moduledoc """
  A server thread dedicated to a user
  """
  use GenServer

  def start(initState, opts \\ []) do
    GenServer.start(__MODULE__, initState, opts)
  end

  def join(server, channelName) do
    GenServer.cast(server, {:join, channelName})
  end

  def leave(server, channelName) do
    GenServer.cast(server, {:leave, channelName})
  end

  def post(server, name, message) do
    GenServer.cast(server, {:post, name, message})
  end

  @doc """
  """
  @impl true
  def init(initState) do
    join(self(), :general)
    {:ok, initState}
  end

  @impl true
  def handle_cast({:join, name}, state) do
    case Storage.get(state.globalChannels, name) do
      nil -> send(state.userAddress, {:channelNotFound, name})
      address -> case Storage.put(state.userChannels, name, address) do
        :ok -> Channel.join(address, state.userName, state.userAddress)
        :not_ok -> send(state.userAddress, {:already_in_channel, name})
      end
    end
    {:noreply, state}
  end

  @impl true
  def handle_cast({:leave, :general}, _state), do: exit(:normal)

  @impl true
  def handle_cast({:leave, name}, state) do
    case Storage.get(state.userChannels, name) do
      nil -> send(state.userAddress, {:channelNotJoined, name})
      address ->
        Storage.delete(state.userChannels, name)
        Channel.leave(address, state.userName)
    end
    {:noreply, state}
  end

  @impl true
  def handle_cast({:post, name, message}, state) do
    case Storage.get(state.userChannels, name) do
      nil -> send(state.userAddress, {:channelNotJoined, name})
      address -> Channel.post(address, {state.userName, message})
    end
    {:noreply, state}
  end

end
