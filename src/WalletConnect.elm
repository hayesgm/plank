port module WalletConnect exposing (connectWC, tryConnectWC)
import Url

port connectWC : String -> Cmd msg

tryConnectWC : String -> Cmd msg
tryConnectWC str =
  case String.startsWith "wc:" str of
    True ->
      connectWC str

    False ->
      Cmd.none

