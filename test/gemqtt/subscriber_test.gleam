import gemqtt.{type Client}
import gemqtt/publisher
import gemqtt/subscriber
import gleam/bit_array
import gleam/dict
import gleam/dynamic.{type Dynamic}
import gleam/erlang/atom
import gleam/erlang/process
import gleam/list
import gleam/option
import gleam/result
import gleeunit
import gleeunit/should
import helper

pub fn main() {
  gleeunit.main()
}

pub fn valid_topic_test() {
  process.flush_messages()

  let topic = "gemqtt/test/subscribe_valid_topic"

  let client = helper.new_test_client("subscribe_valid_topic")
  let assert Ok(Nil) = gemqtt.connect(client)

  let assert Ok(#(option.None, _reasons)) =
    client
    |> subscriber.new
    |> subscriber.add(topics: [topic])
}

pub fn selector_test() {
  process.flush_messages()

  let topic = "gemqtt/test/subscribe_selector"
  let msg_payload = bit_array.from_string("selector payload")

  let client = helper.new_test_client("subscribe_selector")
  let assert Ok(Nil) = gemqtt.connect(client)
  let assert Ok(#(option.None, _)) =
    client
    |> subscriber.new
    |> subscriber.add(topics: [topic])

  // Publish a test message.
  let assert Ok(_) =
    publisher.new(client, topic)
    |> publisher.publish(msg_payload)

  // Attempt to receive that message.
  let assert Ok(got_msg) =
    process.new_selector()
    |> subscriber.selecting_mqtt_messages(Ok)
    |> process.selecting_anything(helper.unexpected_message)
    |> process.select(within: helper.recv_timeout_millis)
    |> result.replace_error("timeout waiting for message")
    |> result.flatten

  got_msg.topic
  |> should.equal(topic)

  got_msg.payload
  |> should.equal(msg_payload)

  got_msg.client
  |> should.equal(client)

  let assert Ok(_) = subscriber.remove(client, [topic])
  let assert Ok(_) = gemqtt.disconnect(client)
}

pub fn local_echo_test() {
  let topic = "gemqtt/test/local_echo"

  // Create subscriber with no local echo.
  let client = helper.new_test_client("no_local_echo")
  let assert Ok(Nil) = gemqtt.connect(client)
  let assert Ok(#(option.None, _)) =
    client
    |> subscriber.new
    |> subscriber.set_local_echo(True)
    |> subscriber.add(topics: [topic])

  // Verify emqtt's `nl` option has the correct value.
  let assert Ok(got_nl) = get_emqtt_sub_option(client, topic, "nl")
  got_nl
  |> should.equal(dynamic.from(0))

  let assert Ok(_) = subscriber.remove(client, [topic])
  let assert Ok(_) = gemqtt.disconnect(client)
}

pub fn no_local_echo_test() {
  let topic = "gemqtt/test/no_local_echo"

  // Create subscriber with no local echo.
  let client = helper.new_test_client("no_local_echo")
  let assert Ok(Nil) = gemqtt.connect(client)
  let assert Ok(#(option.None, _)) =
    client
    |> subscriber.new
    |> subscriber.set_local_echo(False)
    |> subscriber.add(topics: [topic])

  // Verify emqtt's `nl` option has the correct value.
  let assert Ok(got_nl) = get_emqtt_sub_option(client, topic, "nl")
  got_nl
  |> should.equal(dynamic.from(1))

  let assert Ok(_) = subscriber.remove(client, [topic])
  let assert Ok(_) = gemqtt.disconnect(client)
}

fn get_emqtt_sub_option(
  client: Client,
  topic: String,
  opt_name: String,
) -> Result(Dynamic, String) {
  let opt_key = atom.create_from_string(opt_name)

  // Fetch topics & options from emqtt subscription information.
  client
  |> gemqtt.subscriptions
  |> list.key_find(topic)
  |> result.replace_error("topic `" <> topic <> "` not found in subscriptions")
  |> result.then(fn(opts) {
    // Topic was found, get the specified option.
    dict.get(opts, opt_key)
    |> result.replace_error(
      "topic found, but option `" <> opt_name <> "` was not present",
    )
  })
}
