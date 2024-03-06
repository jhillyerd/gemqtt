import gleam/dict.{type Dict}
import gleam/dynamic.{type Dynamic}
import gleam/erlang/atom.{type Atom}
import gleam/erlang/charlist
import gleam/erlang/process.{type Pid}

/// Errors that can occur when working with MQTT connections.
///
pub type Error {
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

pub opaque type Options {
  Options(Dict(Atom, Dynamic))
}

pub type Properties {
  Properties(Dict(String, String))
}

/// MQTT Quality of Service level, controlling how many times a message
/// may be delivered.
///
pub type Qos {
  AtMostOnce
  AtLeastOnce
  ExactlyOnce
}

// TODO: Missing options
// Name
// TcpOpts
// Ssl
// SslOpts
// WsPath
// BridgeMode
// ProtoVer
// Keepalive
// MaxInflight
// RetryInterval
// WillTopic
// WillPayload
// WillRetain
// WillQos
// WillProps
// AckTimeout
// ForcePing
// Properties

pub fn new(host: String) -> Options {
  let host_value =
    host
    |> charlist.from_string
    |> dynamic.from

  let opts =
    dict.new()
    |> dict.insert(atom.create_from_string("host"), host_value)

  Options(opts)
}

pub fn set_auth(opts: Options, username: String, password: String) -> Options {
  opts
  |> set_option(atom.create_from_string("username"), username)
  |> set_option(atom.create_from_string("password"), password)
}

pub fn set_auto_ack(opts: Options, ack: Bool) -> Options {
  set_option(opts, atom.create_from_string("auto_ack"), ack)
}

pub fn set_clean_start(opts: Options, clean: Bool) -> Options {
  set_option(opts, atom.create_from_string("clean_start"), clean)
}

pub fn set_client_id(opts: Options, id: String) -> Options {
  set_option(opts, atom.create_from_string("clientid"), id)
}

pub fn set_connect_timeout(opts: Options, seconds timeout: Int) -> Options {
  set_option(opts, atom.create_from_string("connect_timeout"), timeout)
}

pub fn set_owner(opts: Options, pid: process.Pid) -> Options {
  set_option(opts, atom.create_from_string("owner"), pid)
}

pub fn set_port(opts: Options, port: Int) -> Options {
  set_option(opts, atom.create_from_string("port"), port)
}

/// Client holds the process running the MQTT connection, and is required to
/// publish and subsribe to messages.
///
pub opaque type Client {
  Client(process.Pid)
}

/// Returns the Erlang Pid of the provided Client.
///
pub fn pid_of(client: Client) -> process.Pid {
  let Client(pid) = client
  pid
}

// TODO: Fix dynamic error
/// Configure a client process and link it to ours.  Does not attempt to
/// connect to the MQTT server.
///
@external(erlang, "emqtt_ffi", "start_link")
pub fn start_link(opts: Options) -> Result(Client, Dynamic)

/// Connect to the configured MQTT server.
///
@external(erlang, "emqtt_ffi", "connect")
pub fn connect(client: Client) -> Result(Nil, Error)

// TODO: Fix dynamic error
@external(erlang, "emqtt_ffi", "disconnect")
pub fn disconnect(client: Client) -> Result(Dynamic, Dynamic)

// TODO: Subscription options!
// TODO: Fix dynamic error
@external(erlang, "emqtt_ffi", "subscribe")
pub fn subscribe(client: Client, topic: String) -> Result(Dynamic, Dynamic)

// TODO: Fix dynamic error
@external(erlang, "emqtt_ffi", "unsubscribe")
pub fn unsubscribe(
  client: Client,
  topics: List(String),
) -> Result(Dynamic, Dynamic)

@external(erlang, "emqtt_ffi", "stop")
pub fn stop(client: Pid) -> Result(Nil, Nil)

fn set_option(opts: Options, name: atom.Atom, value: t) -> Options {
  let Options(options) = opts
  Options(dict.insert(options, name, dynamic.from(value)))
}
