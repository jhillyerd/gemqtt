import gemqtt.{type Client}
import gleam/dynamic.{
  type Dynamic, bit_array, bool, field, int, optional_field, string,
}
import gleam/erlang/atom
import gleam/erlang/process
import gleam/option.{type Option}
import gleam/result

pub type Message {
  Message(
    client: Client,
    duplicate: Bool,
    packet_id: Option(Int),
    payload: BitArray,
    qos: Int,
    retain: Bool,
    topic: String,
  )
}

type PublishFieldName {
  ClientPid
  Dup
  PacketId
  Payload
  // Properties
  Qos
  Retain
  Topic
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
  use client <- result.try(field(ClientPid, decode_client)(published))
  use duplicate <- result.try(field(Dup, bool)(published))
  use packet_id <- result.try(optional_field(PacketId, int)(published))
  use payload <- result.try(field(Payload, bit_array)(published))
  use qos <- result.try(field(Qos, int)(published))
  use retain <- result.try(field(Retain, bool)(published))
  use topic <- result.try(field(Topic, string)(published))

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
  fn(published) -> t {
    let assert Ok(message) = message_from_dynamic(published)
    mapper(message)
  }
}

@external(erlang, "emqtt_ffi", "decode_client")
fn decode_client(data: Dynamic) -> Result(Client, List(dynamic.DecodeError))
