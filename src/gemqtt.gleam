import gleam/bit_array
import gleam/dict.{type Dict}
import gleam/dynamic.{type Dynamic}
import gleam/erlang/atom
import gleam/erlang/charlist
import gleam/erlang/os
import gleam/erlang/process
import gleam/io
import gleam/option.{Some}
import gleam/result
import gleam/string
import message

pub fn main() {
  let host = "test.mosquitto.org"

  let assert Ok(client) =
    new(host)
    |> start_link

  let assert Ok(_) = emqtt_connect(client)
  let assert Ok(_) = emqtt_subscribe(client, "#")

  case listen() {
    Ok(_) -> io.print_error("happy exit")
    Error(err) -> io.print_error("Failed: " <> err)
  }

  stop(client)
}

fn listen() -> Result(Nil, String) {
  let publish = atom.create_from_string("publish")

  let got_msg =
    process.new_selector()
    |> process.selecting_record2(publish, fn(published: Dynamic) {
      let msg = message.from_dynamic(published)
      result.map(msg, fn(msg: message.Message) { Publish(msg) })
    })
    |> process.selecting_anything(fn(x: Dynamic) {
      io.println("Unsupported message received:")
      io.debug(x)
      todo("implement this type")
    })
    |> process.select_forever()

  case got_msg {
    Ok(Publish(message)) -> {
      let payload =
        message.payload
        |> bit_array.to_string
        |> result.unwrap("BINARY DATA")

      io.println("Topic: " <> message.topic)
      io.println("Payload: " <> payload)
      io.println("")

      listen()
    }
    Error(err) -> {
      let text = string.inspect(err)
      Error(text)
    }
    Ok(other) -> {
      io.debug(other)
      Error("other message type")
    }
  }
}

// lib code ////////////////////////////////////////

pub type Client

pub type Options {
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

fn with_auth(opts: Options, username: String, password: String) -> Options {
  opts
  |> set_option(Username, username)
  |> set_option(Password, password)
}

fn with_auto_ack(opts: Options, ack: Bool) -> Options {
  set_option(opts, AutoAck, ack)
}

fn with_clean_start(opts: Options, clean: Bool) -> Options {
  set_option(opts, CleanStart, clean)
}

fn with_client_id(opts: Options, id: String) -> Options {
  set_option(opts, Clientid, id)
}

fn with_connect_timeout(opts: Options, seconds timeout: Int) -> Options {
  set_option(opts, ConnectTimeout, timeout)
}

fn set_option(opts: Options, name: EmqttOptionName, value: t) -> Options {
  let Options(options) = opts
  Options(dict.insert(options, name, dynamic.from(value)))
}

pub fn start_link(opts: Options) -> Result(Client, Dynamic) {
  let Options(options) = opts
  emqtt_start_link(options)
}

@external(erlang, "emqtt_ffi", "start_link")
fn emqtt_start_link(
  opts: Dict(EmqttOptionName, Dynamic),
) -> Result(Client, Dynamic)

@external(erlang, "emqtt_ffi", "connect")
fn emqtt_connect(client: Client) -> Result(Dynamic, Dynamic)

@external(erlang, "emqtt_ffi", "subscribe")
fn emqtt_subscribe(client: Client, topic: String) -> Result(Dynamic, Dynamic)

@external(erlang, "emqtt_ffi", "stop")
pub fn stop(client: Client) -> Result(Nil, Nil)

pub type Message {
  Disconnect(reason_code: Dynamic, properties: Dynamic)
  Publish(message: message.Message)
  Puback(data: Dynamic)
}
