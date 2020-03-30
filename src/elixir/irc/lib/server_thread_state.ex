defmodule ServerThreadState do
  @enforce_keys [
    :userName, :userAddress, :userChannels, :globalChannels
  ]
  defstruct [
    userName: nil,
    userAddress: nil,
    userChannels: nil,
    globalChannels: nil
  ]
  @type t :: %__MODULE__{
    userName: atom(),
    userAddress: Process.dest(),
    userChannels: Process.dest(),
    globalChannels: Process.dest()
  }
end
