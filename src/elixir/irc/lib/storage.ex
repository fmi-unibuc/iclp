defmodule Storage do
  @moduledoc """
  An actor wraping a map
  """

  @doc """
  Main loop for Storage
  """

  use Agent

  @doc """
  Starts a new Storage service
  """
  def start_link(opts \\ []) do
    Agent.start_link(fn -> %{} end, opts)
  end

  @doc """
  Assigns a value to key; returns :not_ok if key already present
  """
  def put(storage, key, value) do
    Agent.get_and_update(storage,
      &put_handler(&1, key, value))
  end

  @spec put_handler(map, any, any) :: {:not_ok | :ok, map}
  def put_handler(state, key, value) do
    case Map.get(state, key) do
      nil -> {:ok, Map.put(state, key, value)}
      _value -> {:not_ok, state}
    end
  end

  @doc """
  Assigns a value to key; overwrites existing value
  """
  def put!(storage, key, value) do
    Agent.update(storage, Map, :put, [key, value])
  end

  @doc """
  Gets the value for the key; May return nil
  """
  def get(storage, key) do
    Agent.get(storage, Map, :get, [key])
  end

  @doc """
  Removes the entry corresponding to key
  """
  def delete(storage, key) do
    Agent.update(storage, Map, :delete, [key])
  end

  @doc """
  Executes action for each value in the map
  """
  def foreach(storage, action) do
    Agent.get(storage, &Enum.each(Map.values(&1), action))
  end
end
