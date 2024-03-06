import gemqtt
import gemqtt/publisher
import gemqtt/subscriber
import gleam/bit_array
import gleam/erlang/process
import gleam/function.{identity}
import gleam/result
import gleam/string
import gleeunit
import gleeunit/should

const mqtt_server_port = 41_883

const connect_timeout_seconds = 2

const recv_timeout_millis = 2000

pub fn main() {
  gleeunit.main()
}

pub fn connect_localhost_test() {
  process.trap_exits(True)
  process.flush_messages()

  let assert Ok(client) =
    gemqtt.new("localhost")
    |> gemqtt.set_property("Maximum-Packet-Size", 2048)
    |> gemqtt.set_property("User-Property", #("prop-name", "prop-value"))
    |> gemqtt.set_port(mqtt_server_port)
    |> gemqtt.set_client_id("gemqtt_connect_test")
    |> gemqtt.set_connect_timeout(connect_timeout_seconds)
    |> gemqtt.set_owner(process.self())
    |> gemqtt.start_link

  let assert Ok(Nil) = gemqtt.connect(client)

  let assert Ok(_) = gemqtt.disconnect(client)

  let assert process.ExitMessage(pid: pid, reason: process.Normal) =
    process.new_selector()
    |> process.selecting_trapped_exits(identity)
    |> process.select_forever()

  // Verify exit message was from the process under test.
  client
  |> gemqtt.pid_of
  |> should.equal(pid)

  process.trap_exits(False)
}

pub fn connect_invalid_host_test() {
  process.trap_exits(True)
  process.flush_messages()

  let assert Ok(client) =
    gemqtt.new("invalid.example.com")
    |> gemqtt.set_port(mqtt_server_port)
    |> gemqtt.set_connect_timeout(connect_timeout_seconds)
    |> gemqtt.set_owner(process.self())
    |> gemqtt.start_link

  let assert Error(gemqtt.Nxdomain) = gemqtt.connect(client)

  let assert process.ExitMessage(pid: pid, reason: process.Abnormal(reason)) =
    process.new_selector()
    |> process.selecting_trapped_exits(identity)
    |> process.select_forever()

  reason
  |> should.equal("Shutdown(Nxdomain)")

  // Verify exit message was from the process under test.
  client
  |> gemqtt.pid_of
  |> should.equal(pid)

  process.trap_exits(False)
}

pub fn connect_invalid_port_test() {
  process.trap_exits(True)
  process.flush_messages()

  let assert Ok(client) =
    gemqtt.new("localhost")
    |> gemqtt.set_port(41_999)
    |> gemqtt.set_connect_timeout(connect_timeout_seconds)
    |> gemqtt.set_owner(process.self())
    |> gemqtt.start_link

  let assert Error(gemqtt.Econnrefused) = gemqtt.connect(client)

  let assert process.ExitMessage(pid: pid, reason: process.Abnormal(reason)) =
    process.new_selector()
    |> process.selecting_trapped_exits(identity)
    |> process.select_forever()

  reason
  |> should.equal("Shutdown(Econnrefused)")

  // Verify exit message was from the process under test.
  client
  |> gemqtt.pid_of
  |> should.equal(pid)

  process.trap_exits(False)
}

pub fn roundtrip_test() {
  let topic = "gemqtt/test/roundtrip"
  let msg_content = bit_array.from_string("roundtrip content")

  let assert Ok(client) =
    gemqtt.new("localhost")
    |> gemqtt.set_port(mqtt_server_port)
    |> gemqtt.set_connect_timeout(connect_timeout_seconds)
    |> gemqtt.start_link

  let assert Ok(Nil) = gemqtt.connect(client)

  let assert Ok(_) = gemqtt.subscribe(client, topic)

  // Publish a test message.
  let assert Ok(_) =
    publisher.new(client, topic)
    |> publisher.publish(msg_content)

  // Attempt to receive that message.
  let assert Ok(got_msg) =
    process.new_selector()
    |> subscriber.selecting_mqtt_messages(Ok)
    |> process.selecting_anything(unexpected_message)
    |> process.select(within: recv_timeout_millis)
    |> result.replace_error("timeout waiting for message")
    |> result.flatten

  got_msg.topic
  |> should.equal(topic)

  got_msg.payload
  |> should.equal(msg_content)

  got_msg.client
  |> should.equal(client)

  let assert Ok(_) = gemqtt.unsubscribe(client, [topic])
  let assert Ok(_) = gemqtt.disconnect(client)
}

fn unexpected_message(msg) {
  Error("unexpected message: " <> string.inspect(msg))
}
