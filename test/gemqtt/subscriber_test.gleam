import gemqtt
import gemqtt/publisher
import gemqtt/subscriber
import gleam/bit_array
import gleam/erlang/process
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
    subscriber.add(client, opts: [], topics: [topic])
}

pub fn selector_test() {
  process.flush_messages()

  let topic = "gemqtt/test/subscribe_selector"
  let msg_payload = bit_array.from_string("selector payload")

  let client = helper.new_test_client("subscribe_selector")
  let assert Ok(Nil) = gemqtt.connect(client)
  let assert Ok(#(option.None, _)) =
    subscriber.add(client, opts: [], topics: [topic])

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
