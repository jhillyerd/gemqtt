import gemqtt.{type Client, type Properties, Properties}
import gleam/dict.{type Dict}
import gleam/dynamic.{type Dynamic}
import gleam/erlang/atom.{type Atom}
import gleam/option

// TODO: Topic String|Charlist|Binary type?
pub type Publisher {
  Publisher(
    client: Client,
    topic: String,
    options: List(PublishOption),
    properties: Properties,
  )
}

pub type PublishOption {
  Retain(Bool)
  Qos(gemqtt.Qos)
}

pub fn new(client: Client, topic: String) -> Publisher {
  Publisher(
    client: client,
    topic: topic,
    options: [],
    properties: Properties(dict.new()),
  )
}

pub fn set_qos(publisher: Publisher, qos: gemqtt.Qos) -> Publisher {
  let opts = [Qos(qos), ..publisher.options]
  Publisher(..publisher, options: opts)
}

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
