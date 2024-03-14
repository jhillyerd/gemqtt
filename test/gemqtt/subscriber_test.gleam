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

// local_echo is inverted to set "no local" value
pub fn local_echo_test() {
  // Test cases, of pattern #(tc_name, input_value, want_value)
  let tcs = [#("false", False, 1), #("true", True, 0)]

  with_client("local_echo_test", fn(client, topic) {
    list.each(tcs, fn(tc) {
      let #(tc_name, input_value, want_value) = tc

      // Use unique topic for this sub-test.
      let topic = topic <> "/" <> tc_name

      // Create subscriber and apply test case fn to it.
      let assert Ok(#(option.None, _)) =
        client
        |> subscriber.new
        |> subscriber.set_local_echo(input_value)
        |> subscriber.add(topics: [topic])

      // Verify the internal emqtt option has the correct value.
      let assert Ok(got_value) = get_emqtt_sub_option(client, topic, "nl")
      #(tc_name, got_value)
      |> should.equal(#(tc_name, dynamic.from(want_value)))
    })
  })
}

pub fn qos_test() {
  // Test cases, of pattern #(tc_name, input_value, want_value)
  let tcs = [
    #("at_most_once", gemqtt.AtMostOnce, 0),
    #("at_least_once", gemqtt.AtLeastOnce, 1),
    #("exactly_once", gemqtt.ExactlyOnce, 2),
  ]

  with_client("qos_test", fn(client, topic) {
    list.each(tcs, fn(tc) {
      let #(tc_name, input_value, want_value) = tc

      // Use unique topic for this sub-test.
      let topic = topic <> "/" <> tc_name

      // Create subscriber and apply test case fn to it.
      let assert Ok(#(option.None, _)) =
        client
        |> subscriber.new
        |> subscriber.set_qos(input_value)
        |> subscriber.add(topics: [topic])

      // Verify the internal emqtt option has the correct value.
      let assert Ok(got_value) = get_emqtt_sub_option(client, topic, "qos")
      #(tc_name, got_value)
      |> should.equal(#(tc_name, dynamic.from(want_value)))
    })
  })
}

pub fn retain_as_published_test() {
  // Test cases, of pattern #(tc_name, input_value, want_value)
  let tcs = [#("false", False, 0), #("true", True, 1)]

  with_client("retain_as_published_test", fn(client, topic) {
    list.each(tcs, fn(tc) {
      let #(tc_name, input_value, want_value) = tc

      // Use unique topic for this sub-test.
      let topic = topic <> "/" <> tc_name

      // Create subscriber and apply test case fn to it.
      let assert Ok(#(option.None, _)) =
        client
        |> subscriber.new
        |> subscriber.set_retain_as_published(input_value)
        |> subscriber.add(topics: [topic])

      // Verify the internal emqtt option has the correct value.
      let assert Ok(got_value) = get_emqtt_sub_option(client, topic, "rap")
      #(tc_name, got_value)
      |> should.equal(#(tc_name, dynamic.from(want_value)))
    })
  })
}

pub fn retain_handling_test() {
  // Test cases, of pattern #(tc_name, input_value, want_value)
  let tcs = [
    #("always", subscriber.SentAlways, 0),
    #("on_new", subscriber.SentOnNewSubscription, 1),
    #("never", subscriber.SentNever, 2),
  ]

  with_client("retain_handling_test", fn(client, topic) {
    list.each(tcs, fn(tc) {
      let #(tc_name, input_value, want_value) = tc

      // Use unique topic for this sub-test.
      let topic = topic <> "/" <> tc_name

      // Create subscriber and apply test case fn to it.
      let assert Ok(#(option.None, _)) =
        client
        |> subscriber.new
        |> subscriber.set_retain_handling(input_value)
        |> subscriber.add(topics: [topic])

      // Verify the internal emqtt option has the correct value.
      let assert Ok(got_value) = get_emqtt_sub_option(client, topic, "rh")
      #(tc_name, got_value)
      |> should.equal(#(tc_name, dynamic.from(want_value)))
    })
  })
}

// Creates and connects a new gemqtt Client, and passes it with a generated
// topic to the provided function.
fn with_client(test_id: String, handler: fn(Client, String) -> Nil) {
  process.trap_exits(False)
  process.flush_messages()

  let topic = "gemqtt/test/subscriber/" <> test_id
  let client = helper.new_test_client(test_id)
  let assert Ok(Nil) = gemqtt.connect(client)

  handler(client, topic)

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
