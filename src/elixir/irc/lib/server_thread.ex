defmodule ServerThread do
  @moduledoc """
  A server thread dedicated to a user
  """

  @doc """
  """
  @spec init(Process.dest(), Process.dest(), atom(), Process.dest()) :: any()
  def init(server, channels, name, address) do
    localChannels = spawn(Storage, :init, [])
    id = self()
    send(id, {:join, :general})
    send(server, {:registered, name, address, id, localChannels})
    loop(%ServerThreadState{
      userName: name,
      userAddress: address,
      globalChannels: channels,
      userChannels: localChannels
    })
  end

  defp loop(state) do
    receive do
      {:join, channelName} ->
        send(state.globalChannels,
          {:get, channelName, &handleJoin(state, channelName, &1)})
      {:leave, channelName} ->
        send(state.userChannels,
          {:get, channelName, &handleLeave(state, channelName, &1)})
      {:post, channelName, message} ->
        send(state.userChannels,
          {:get, channelName, &handlePost(state, channelName, message, &1)})
    end
    loop(state)
  end

  defp handleJoin(state, name, address) do
    case address do
      nil -> send(state.userAddress, {:channelNotFound, name})
      _ ->
        send(state.userChannels,
          {:put, name, address, &handleFirstJoin(state, name, address, &1)})
    end
  end

  defp handleFirstJoin(state, name, address, answer) do
    case answer do
      :ok -> send(address, {:join, state.userName, state.userAddress})
      :not_ok -> send(state.userAddress, {:already_in_channel, name})
    end
  end

  defp handleLeave(_state, :general, _address), do: exit(:normal)
  defp handleLeave(state, name, address) do
    case address do
      nil -> send(state.userAddress, {:channelNotJoined, name})
      _ ->
        send(state.userChannels, {:delete, name})
        send(address, {:leave, state.userName})
    end
  end

  defp handlePost(state, name, message, address) do
    case address do
      nil -> send(state.userAddress, {:channelNotJoined, name})
      _ -> send(address, {:post, {state.userName, message}})
    end
  end


end
