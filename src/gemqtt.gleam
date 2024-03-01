import gleam/dict.{type Dict}
import gleam/dynamic.{type Dynamic}
import gleam/erlang/charlist
import gleam/erlang/process.{type Pid}

pub opaque type Options {
  Options(Dict(EmqttOptionName, Dynamic))
}

pub type EmqttOptionName {
  Name
  Owner
  Host
  Port
  TcpOpts
  Ssl
  SslOpts
  WsPath
  ConnectTimeout
  BridgeMode
  Clientid
  CleanStart
  Username
  Password
  ProtoVer
  Keepalive
  MaxInflight
  RetryInterval
  WillTopic
  WillPayload
  WillRetain
  WillQos
  WillProps
  AutoAck
  AckTimeout
  ForcePing
  Properties
}

/// Errors that can occur when working with TCP sockets.
///
/// For more information on these errors see the Erlang documentation:
/// - https://www.erlang.org/doc/man/inet#type-posix
///
pub type ConnectError {
  // https://www.erlang.org/doc/man/inet#type-posix
  Closed
  Timeout
  Eaddrinuse
  Eaddrnotavail
  Eafnosupport
  Ealready
  Econnaborted
  Econnrefused
  Econnreset
  Edestaddrreq
  Ehostdown
  Ehostunreach
  Einprogress
  Eisconn
  Emsgsize
  Enetdown
  Enetunreach
  Enopkg
  Enoprotoopt
  Enotconn
  Enotty
  Enotsock
  Eproto
  Eprotonosupport
  Eprototype
  Esocktnosupport
  Etimedout
  Ewouldblock
  Exbadport
  Exbadseq
  Nxdomain
}

pub fn new(host: String) -> Options {
  let host_value =
    host
    |> charlist.from_string
    |> dynamic.from

  let opts =
    dict.new()
    |> dict.insert(Host, host_value)

  Options(opts)
}

pub fn set_auth(opts: Options, username: String, password: String) -> Options {
  opts
  |> set_option(Username, username)
  |> set_option(Password, password)
}

pub fn set_auto_ack(opts: Options, ack: Bool) -> Options {
  set_option(opts, AutoAck, ack)
}

pub fn set_clean_start(opts: Options, clean: Bool) -> Options {
  set_option(opts, CleanStart, clean)
}

pub fn set_client_id(opts: Options, id: String) -> Options {
  set_option(opts, Clientid, id)
}

pub fn set_connect_timeout(opts: Options, seconds timeout: Int) -> Options {
  set_option(opts, ConnectTimeout, timeout)
}

pub fn set_owner(opts: Options, pid: process.Pid) -> Options {
  set_option(opts, Owner, pid)
}

pub fn set_port(opts: Options, port: Int) -> Options {
  set_option(opts, Port, port)
}

@external(erlang, "emqtt_ffi", "start_link")
pub fn start_link(opts: Options) -> Result(Pid, Dynamic)

@external(erlang, "emqtt_ffi", "connect")
pub fn connect(client: Pid) -> Result(Nil, ConnectError)

@external(erlang, "emqtt_ffi", "disconnect")
pub fn disconnect(client: Pid) -> Result(Dynamic, Dynamic)

// TODO: Subscription options!
@external(erlang, "emqtt_ffi", "subscribe")
pub fn subscribe(client: Pid, topic: String) -> Result(Dynamic, Dynamic)

@external(erlang, "emqtt_ffi", "unsubscribe")
pub fn unsubscribe(
  client: Pid,
  topics: List(String),
) -> Result(Dynamic, Dynamic)

// TODO: Publish options!
// TODO: Topic String|Charlist|Binary type?
pub fn publish(
  client: Pid,
  topic: String,
  payload: BitArray,
) -> Result(Dynamic, Dynamic) {
  publish_(client, topic, payload)
}

@external(erlang, "emqtt_ffi", "publish")
pub fn publish_(
  client: Pid,
  topic: String,
  payload: BitArray,
) -> Result(Dynamic, Dynamic)

@external(erlang, "emqtt_ffi", "stop")
pub fn stop(client: Pid) -> Result(Nil, Nil)

fn set_option(opts: Options, name: EmqttOptionName, value: t) -> Options {
  let Options(options) = opts
  Options(dict.insert(options, name, dynamic.from(value)))
}
