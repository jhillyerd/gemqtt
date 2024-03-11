import gemqtt.{type Client, type Properties}
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
  published: Dynamic,
) -> Result(Message, List(dynamic.DecodeError)) {
  use client <- try(field(publish.ClientPid, decode_client)(published))
  use duplicate <- try(field(publish.Dup, bool)(published))
  use packet_id <- try(optional_field(publish.PacketId, int)(published))
  use payload <- try(field(publish.Payload, bit_array)(published))
  use qos <- try(field(publish.Qos, decode_qos)(published))
  use retain <- try(field(publish.Retain, bool)(published))
  use topic <- try(field(publish.Topic, string)(published))

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

pub type SubscribeOption {
  // No Local
  Nl(Bool)
  // Quality of Service
  Qos(gemqtt.Qos)
  // Retain as Published
  Rap(Bool)
  // Retain Handling
  Rh(Int)
}

// TODO: Subscription options!
@external(erlang, "emqtt_ffi", "subscribe")
pub fn add(
  client: Client,
  opts options: List(SubscribeOption),
  topics topics: List(String),
) -> Result(#(Option(Properties), List(Int)), Nil)

// TODO: Fix dynamic error
@external(erlang, "emqtt_ffi", "unsubscribe")
pub fn remove(client: Client, topics: List(String)) -> Result(Dynamic, Dynamic)

// Calls mapper fn after decoding `Message`, or crashes.
fn map_dynamic_message(mapper: fn(Message) -> t) -> fn(Dynamic) -> t {
  fn(published) -> t {
    let assert Ok(message) = message_from_dynamic(published)
    mapper(message)
  }
}

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
