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
  Options(options: Dict(Atom, Dynamic), properties: Properties)
}

pub type Properties {
  Properties(Dict(Atom, Dynamic))
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

  Options(options: opts, properties: Properties(dict.new()))
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

/// Sets an MQTT CONNECT packet property.  Please note that properties are
/// validated when `start_link` is called; if _emqtt_ detects errors it will
/// cause your process to fail rather than return an Error.
///
/// Available connect properties:
///
/// - `Session-Expiry-Interval`: Four byte integer
/// - `Receive-Maximum`: Two byte integer
/// - `Maximum-Packet-Size`: Four byte integer
/// - `Topic-Alias-Maximum`: Two byte integer
/// - `Request-Response-Information`: Byte
/// - `Request-Problem-Information`: Byte
/// - `User-Property`: UTF-8 string pair
/// - `Authentication-Method`: UTF-8 encoded string
/// - `Authentication-Data`: Binary data
///
/// Example usage:
///
/// ```
/// gemqtt.new("localhost")
/// |> gemqtt.set_property("Maximum-Packet-Size", 300)
/// |> gemqtt.set_property("User-Property", #("prop-name", "prop-value"))
/// ```
///
pub fn set_property(opts: Options, name: String, value: t) -> Options {
  let Options(options: _, properties: Properties(props)) = opts
  Options(
    ..opts,
    properties: Properties(dict.insert(
      props,
      atom.create_from_string(name),
      dynamic.from(value),
    )),
  )
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
pub fn start_link(opts: Options) -> Result(Client, Dynamic) {
  let Options(options: options, properties: Properties(properties)) = opts
  let options =
    dict.insert(
      options,
      atom.create_from_string("properties"),
      dynamic.from(properties),
    )
  start_link_(options)
}

@external(erlang, "emqtt_ffi", "start_link")
fn start_link_(opts: Dict(Atom, Dynamic)) -> Result(Client, Dynamic)

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
  Options(..opts, options: dict.insert(opts.options, name, dynamic.from(value)))
}
