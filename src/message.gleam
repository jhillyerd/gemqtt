import gleam/dynamic.{
  type Dynamic, bit_array, bool, field, int, optional_field, string,
}
import gleam/option.{type Option}
import gleam/result

// TODO import loop
pub type Client

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

pub type PublishField {
  ClientPid
  Dup
  PacketId
  Payload
  Properties
  Qos
  Retain
  Topic
}

pub fn from_dynamic(
  published: Dynamic,
) -> Result(Message, List(dynamic.DecodeError)) {
  use client <- result.try(field(ClientPid, dynamic.dynamic)(published))
  use duplicate <- result.try(field(Dup, bool)(published))
  use packet_id <- result.try(optional_field(PacketId, int)(published))
  use payload <- result.try(field(Payload, bit_array)(published))
  use qos <- result.try(field(Qos, int)(published))
  use retain <- result.try(field(Retain, bool)(published))
  use topic <- result.try(field(Topic, string)(published))

  Ok(Message(
    client: dynamic.unsafe_coerce(client),
    duplicate: duplicate,
    packet_id: packet_id,
    payload: payload,
    qos: qos,
    retain: retain,
    topic: topic,
  ))
}
