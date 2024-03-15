import gemqtt.{type Client, type Properties}
import gemqtt/ffi/puback
import gemqtt/ffi/publish
import gleam/dynamic.{
  type Dynamic, bit_array, bool, field, int, optional_field, string,
}
import gleam/erlang/atom
import gleam/erlang/process
import gleam/int
import gleam/option.{type Option}
import gleam/result.{try}

pub type Message {
  Message(
    client: Client,
    duplicate: Bool,
    packet_id: Option(Int),
    payload: BitArray,
    qos: gemqtt.Qos,
    retain: Bool,
    topic: String,
  )
}

/// Configure a selector to receive messages from MQTT clients.
///
/// Note this will receive messages from all MQTT clients that the process
/// controls, rather than any specific one. If you wish to only handle
/// messages from one client then use one process per client.
///
pub fn selecting_mqtt_messages(
  selector: process.Selector(t),
  mapper: fn(Message) -> t,
) -> process.Selector(t) {
  let publish = atom.create_from_string("publish")

  selector
  |> process.selecting_record2(publish, map_dynamic_message(mapper))
}

/// Decodes a message received by emqtt. Prefer fn `selecting_mqtt_messages`
/// over using this directly.
///
pub fn message_from_dynamic(
  input: Dynamic,
) -> Result(Message, List(dynamic.DecodeError)) {
  use client <- try(field(publish.ClientPid, decode_client)(input))
  use duplicate <- try(field(publish.Dup, bool)(input))
  use packet_id <- try(optional_field(publish.PacketId, int)(input))
  use payload <- try(field(publish.Payload, bit_array)(input))
  use qos <- try(field(publish.Qos, decode_qos)(input))
  use retain <- try(field(publish.Retain, bool)(input))
  use topic <- try(field(publish.Topic, string)(input))

  Ok(Message(
    client: client,
    duplicate: duplicate,
    packet_id: packet_id,
    payload: payload,
    qos: qos,
    retain: retain,
    topic: topic,
  ))
}

// Calls mapper fn after decoding `Message`, or crashes.
fn map_dynamic_message(mapper: fn(Message) -> t) -> fn(Dynamic) -> t {
  fn(input) -> t {
    let assert Ok(message) = message_from_dynamic(input)
    mapper(message)
  }
}

pub type PubAck {
  PubAck(packet_id: Int, reason_code: Int)
}

/// Configure a selector to receive pub-acks from MQTT clients.
///
/// Note this will receive pub-acks from all MQTT clients that the process
/// controls, rather than any specific one.
///
pub fn selecting_mqtt_pubacks(
  selector: process.Selector(t),
  mapper: fn(PubAck) -> t,
) -> process.Selector(t) {
  let puback = atom.create_from_string("puback")

  selector
  |> process.selecting_record2(puback, map_dynamic_puback(mapper))
}

/// Decodes a pub-ack received by emqtt. Prefer fn `selecting_mqtt_pubacks`
/// over using this directly.
///
pub fn puback_from_dynamic(
  input: Dynamic,
) -> Result(PubAck, List(dynamic.DecodeError)) {
  use packet_id <- try(field(puback.PacketId, int)(input))
  use reason_code <- try(field(puback.ReasonCode, int)(input))

  Ok(PubAck(packet_id: packet_id, reason_code: reason_code))
}

// Calls mapper fn after decoding `Message`, or crashes.
fn map_dynamic_puback(mapper: fn(PubAck) -> t) -> fn(Dynamic) -> t {
  fn(input) -> t {
    let assert Ok(puback) = puback_from_dynamic(input)
    mapper(puback)
  }
}

/// Controls whether retained messages are sent when a subscription is added.
pub type RetainHandling {
  /// Retained messages are sent whenever a subscription is established.
  SentAlways

  /// Retained messages are sent only when establishing a new subscription,
  /// not a repeated one.
  SentOnNewSubscription

  /// No retained messages are sent when a subscription is established.
  SentNever
}

pub opaque type Subscriber {
  Subscriber(
    client: Client,
    no_local: Bool,
    qos: gemqtt.Qos,
    retain_as_published: Bool,
    retain_handling: RetainHandling,
  )
}

/// Creates a new subscriber with default options:
///
/// - no_local: False
/// - qos: AtMostOnce
/// - retain_as_published: False
/// - retain_handling: SentAlways
///
pub fn new(client: Client) -> Subscriber {
  Subscriber(
    client: client,
    no_local: False,
    qos: gemqtt.AtMostOnce,
    retain_as_published: False,
    retain_handling: SentAlways,
  )
}

pub fn set_local_echo(subscriber: Subscriber, value: Bool) -> Subscriber {
  Subscriber(..subscriber, no_local: !value)
}

pub fn set_qos(subscriber: Subscriber, value: gemqtt.Qos) -> Subscriber {
  Subscriber(..subscriber, qos: value)
}

pub fn set_retain_as_published(
  subscriber: Subscriber,
  value: Bool,
) -> Subscriber {
  Subscriber(..subscriber, retain_as_published: value)
}

pub fn set_retain_handling(
  subscriber: Subscriber,
  value: RetainHandling,
) -> Subscriber {
  Subscriber(..subscriber, retain_handling: value)
}

type SubscribeOption {
  // No Local
  Nl(Bool)
  // Quality of Service
  Qos(gemqtt.Qos)
  // Retain as Published
  Rap(Bool)
  // Retain Handling
  Rh(Int)
}

// TODO Subscription properties!
pub fn add(
  sub: Subscriber,
  topics topics: List(String),
) -> Result(#(Option(Properties), List(Int)), Nil) {
  let rh = case sub.retain_handling {
    SentAlways -> 0
    SentOnNewSubscription -> 1
    SentNever -> 2
  }

  let opts = [
    Nl(sub.no_local),
    Qos(sub.qos),
    Rap(sub.retain_as_published),
    Rh(rh),
  ]

  add_(sub.client, opts, topics)
}

@external(erlang, "emqtt_ffi", "subscribe")
fn add_(
  client: Client,
  opts options: List(SubscribeOption),
  topics topics: List(String),
) -> Result(#(Option(Properties), List(Int)), Nil)

@external(erlang, "emqtt_ffi", "unsubscribe")
pub fn remove(
  client: Client,
  topics: List(String),
) -> Result(#(Option(Properties), List(Int)), Nil)

@external(erlang, "emqtt_ffi", "decode_client")
fn decode_client(data: Dynamic) -> Result(Client, List(dynamic.DecodeError))

fn decode_qos(data: Dynamic) -> Result(gemqtt.Qos, dynamic.DecodeErrors) {
  data
  |> int
  |> result.try(fn(qos) {
    case qos {
      0 -> Ok(gemqtt.AtMostOnce)
      1 -> Ok(gemqtt.AtLeastOnce)
      2 -> Ok(gemqtt.ExactlyOnce)
      _ -> Error([dynamic.DecodeError("0,1,2", int.to_string(qos), [])])
    }
  })
}
