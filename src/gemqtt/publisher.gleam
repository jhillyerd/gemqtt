import gemqtt.{type Properties, Properties}
import gleam/dict.{type Dict}
import gleam/erlang/process.{type Pid}
import gleam/option

// TODO: Topic String|Charlist|Binary type?
pub type Publisher {
  Publisher(
    client: Pid,
    topic: String,
    options: List(PublishOption),
    properties: Properties,
  )
}

pub type PublishOption {
  Retain(Bool)
  Qos(gemqtt.Qos)
}

pub fn new(client: Pid, topic: String) -> Publisher {
  Publisher(
    client: client,
    topic: topic,
    options: [],
    properties: Properties(dict.new()),
  )
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
  client: Pid,
  topic: String,
  props: Dict(String, String),
  payload: BitArray,
  opts: List(PublishOption),
) -> Result(option.Option(Int), gemqtt.Error)
