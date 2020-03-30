defmodule ChannelState do
  @enforce_keys [:name, :users]
  defstruct name: nil, users: nil
  @type t :: %__MODULE__{
    name: atom(),
    users: Process.dest()
  }
end
