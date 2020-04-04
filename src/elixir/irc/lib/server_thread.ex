defmodule ServerThread do
  @moduledoc """
  A server thread dedicated to a user
  """

  @doc """
  """
  def init(channels, name, address) do
    {:ok, localChannels} = Storage.start_link()
    initState =
      %ServerThreadState{
        userName: name,
        userAddress: address,
        globalChannels: channels,
        userChannels: localChannels
      }
    id = spawn(fn -> loop(initState) end)
    send(id, {:join, :general})
    {id, localChannels}
  end

  defp loop(state) do
    receive do
      {:join, name} ->
        case Storage.get(state.globalChannels, name) do
          nil -> send(state.userAddress, {:channelNotFound, name})
          address -> case Storage.put(state.userChannels, name, address) do
            :ok -> send(address, {:join, state.userName, state.userAddress})
            :not_ok -> send(state.userAddress, {:already_in_channel, name})
          end
        end
      {:leave, :general} -> exit(:normal)
      {:leave, name} ->
        case Storage.get(state.userChannels, name) do
          nil -> send(state.userAddress, {:channelNotJoined, name})
          address ->
            Storage.delete(state.userChannels, name)
            send(address, {:leave, state.userName})
        end
      {:post, name, message} ->
        case Storage.get(state.userChannels, name) do
          nil -> send(state.userAddress, {:channelNotJoined, name})
          address -> send(address, {:post, {state.userName, message}})
        end
    end
    loop(state)
  end
end
