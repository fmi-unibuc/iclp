defmodule Storage do
  @moduledoc """
  An actor wraping a map
  """

  @doc """
  Main loop for Storage
  """
  def init(state \\ %{}) do
    receive do
      {:put, key, value, action} ->
        case Map.get(state, key) do
          nil ->
            action.(:ok)
            init(Map.put(state, key, value))
          _value ->
            action.(:not_ok)
            init(state)
        end
      {:put!, key, value} ->
        init(Map.put(state, key, value))
      {:get, key, action} ->
        action.(Map.get(state, key))
        init(state)
      {:delete, key} ->
        init(Map.delete(state, key))
      {:foreach, action} ->
        Enum.each(Map.values(state), action)
        init(state)
    end
  end

end
