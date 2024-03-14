import gemqtt.{type Client}
import gemqtt/publisher
import gemqtt/subscriber
import gleam/bit_array
import gleam/erlang/process
import gleam/option
import gleeunit
import gleeunit/should
import helper

pub fn main() {
  gleeunit.main()
}

pub fn default_qos_test() {
  with_subscription("publish_default_qos", fn(client, topic) {
    let msg_payload = bit_array.from_string("publish test payload")

    // Publish a test message.
    let assert Ok(_) =
      publisher.new(client, topic)
      |> publisher.publish(msg_payload)

    let got_msg = helper.get_message()

    got_msg.client
    |> should.equal(client)

    // Verifies the default message QoS value, ensuring other QoS tests
    // actually change the value.
    got_msg.qos
    |> should.equal(gemqtt.AtMostOnce)
  })
}

pub fn atleast_qos_test() {
  with_subscription("publish_atleast_qos", fn(client, topic) {
    let msg_payload = bit_array.from_string("publish test payload")

    // Publish a test message.
    let assert Ok(_) =
      publisher.new(client, topic)
      |> publisher.set_qos(gemqtt.AtLeastOnce)
      |> publisher.publish(msg_payload)

    let got_msg = helper.get_message()

    got_msg.client
    |> should.equal(client)

    // Verifies the default message QoS value, ensuring other QoS tests
    // actually change the value.
    got_msg.qos
    |> should.equal(gemqtt.AtLeastOnce)
  })
}

pub fn exactly_qos_test() {
  with_subscription("publish_exactly_qos", fn(client, topic) {
    let msg_payload = bit_array.from_string("publish test payload")

    // Publish a test message.
    let assert Ok(_) =
      publisher.new(client, topic)
      |> publisher.set_qos(gemqtt.ExactlyOnce)
      |> publisher.publish(msg_payload)

    let got_msg = helper.get_message()

    got_msg.client
    |> should.equal(client)

    // Verifies the default message QoS value, ensuring other QoS tests
    // actually change the value.
    got_msg.qos
    |> should.equal(gemqtt.ExactlyOnce)
  })
}

// Creates and connects a new gemqtt Client, then subscribes to a topic
// created from `test_id`, passing both the a handler function to perform
// the actual test logic.
fn with_subscription(test_id: String, handler: fn(Client, String) -> Nil) {
  process.trap_exits(False)
  process.flush_messages()

  let topic = "gemqtt/test/publisher/" <> test_id
  let client = helper.new_test_client(test_id)
  let assert Ok(Nil) = gemqtt.connect(client)
  let assert Ok(#(option.None, _)) =
    client
    |> subscriber.new
    |> subscriber.set_qos(gemqtt.ExactlyOnce)
    |> subscriber.add(topics: [topic])

  handler(client, topic)

  let assert Ok(_) = subscriber.remove(client, [topic])
  let assert Ok(_) = gemqtt.disconnect(client)
}
