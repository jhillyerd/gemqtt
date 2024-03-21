//// Module for publishing messages to an MQTT server.
////
//// Example usage:
////
////     let msg_payload = bit_array.from_string("test payload")
////
////     let assert Ok(_) =
////       publisher.new(client, "my/topic")
////       |> publisher.set_qos(gemqtt.AtLeastOnce)
////       |> publisher.publish(msg_payload)

import gemqtt.{type Client, type Properties, Properties}
import gleam/dict.{type Dict}
import gleam/dynamic.{type Dynamic}
import gleam/erlang/atom.{type Atom}
import gleam/option

/// Publisher holds the configuration for publishing to a particular topic.
/// Create it with the `new` function.
///
pub type Publisher {
  Publisher(
    client: Client,
    topic: String,
    options: List(PublishOption),
    properties: Properties,
  )
}

// TODO Privatize or opaque
pub type PublishOption {
  Retain(Bool)
  Qos(gemqtt.Qos)
}

/// Creates a new publisher for the specified topic.
///
pub fn new(client: Client, topic: String) -> Publisher {
  Publisher(
    client: client,
    topic: topic,
    options: [],
    properties: Properties(dict.new()),
  )
}

/// Sets the QoS level for published messages; defaults to `AtMostOnce`.
///
pub fn set_qos(publisher: Publisher, qos: gemqtt.Qos) -> Publisher {
  let opts = [Qos(qos), ..publisher.options]
  Publisher(..publisher, options: opts)
}

/// Indicates to the server whether the most recently published message should
/// be retained for future subscribers as a _last known good_ message on this
/// topic.  Defaults to false.
///
pub fn set_retain(publisher: Publisher, retain: Bool) -> Publisher {
  let opts = [Retain(retain), ..publisher.options]
  Publisher(..publisher, options: opts)
}

/// Sends a PUBLISH packet to the server.
///
/// Upon success, will returns `Ok(Some(packet ID))` if QoS is `AtLeastOnce` or
/// `ExactlyOnce`, otherwise `Ok(None)`.
///
pub fn publish(
  publisher: Publisher,
  payload: BitArray,
) -> Result(option.Option(Int), gemqtt.Error) {
  let Publisher(
    client: client,
    topic: topic,
    options: pub_opts,
    properties: Properties(props),
  ) = publisher

  publish_(client, topic, props, payload, pub_opts)
}

@external(erlang, "emqtt_ffi", "publish")
fn publish_(
  client: Client,
  topic: String,
  props: Dict(Atom, Dynamic),
  payload: BitArray,
  opts: List(PublishOption),
) -> Result(option.Option(Int), gemqtt.Error)
