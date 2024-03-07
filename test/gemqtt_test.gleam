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

// External test MQTT server host.  Must accept anonymous connections.
const mqtt_server_host = "localhost"

// External test MQTT server port.
const mqtt_server_port = 41_883

// Timeout when initially connecting to the MQTT server.
const connect_timeout_seconds = 2

// Timeout waiting for a message to be received from the MQTT server.
const recv_timeout_millis = 2000

pub fn main() {
  gleeunit.main()
}

pub fn connect_test() {
  process.trap_exits(True)
  process.flush_messages()

  let client = new_test_client("connect_test")
  let assert Ok(Nil) = gemqtt.connect(client)
  let assert Ok(_) = gemqtt.disconnect(client)
  should_exit_normally(client)

  process.trap_exits(False)
}

pub fn client_server_name_test() {
  process.trap_exits(False)

  let assert Ok(client) =
    gemqtt.new(mqtt_server_host)
    |> gemqtt.set_port(mqtt_server_port)
    |> gemqtt.set_connect_timeout(connect_timeout_seconds)
    |> gemqtt.set_name("gemqtt_dup")
    |> gemqtt.start_link

  // Start another Client with the same Erlang server name.
  let assert Error(gemqtt.AlreadyStarted(orig_pid)) =
    gemqtt.new(mqtt_server_host)
    |> gemqtt.set_port(mqtt_server_port)
    |> gemqtt.set_connect_timeout(connect_timeout_seconds)
    |> gemqtt.set_name("gemqtt_dup")
    |> gemqtt.start_link

  orig_pid
  |> should.equal(gemqtt.pid_of(client))

  let assert Ok(Nil) = gemqtt.stop(client)
}

pub fn connect_invalid_host_test() {
  process.trap_exits(True)
  process.flush_messages()

  let assert Ok(client) =
    gemqtt.new("invalid.example.com")
    |> gemqtt.set_port(mqtt_server_port)
    |> gemqtt.set_connect_timeout(connect_timeout_seconds)
    |> gemqtt.start_link

  let assert Error(gemqtt.Nxdomain) = gemqtt.connect(client)

  client
  |> should_exit_abnormally
  |> should.equal("Shutdown(Nxdomain)")

  process.trap_exits(False)
}

pub fn connect_invalid_port_test() {
  process.trap_exits(True)
  process.flush_messages()

  let assert Ok(client) =
    gemqtt.new(mqtt_server_host)
    |> gemqtt.set_port(41_999)
    |> gemqtt.set_connect_timeout(connect_timeout_seconds)
    |> gemqtt.start_link

  let assert Error(gemqtt.Econnrefused) = gemqtt.connect(client)

  client
  |> should_exit_abnormally
  |> should.equal("Shutdown(Econnrefused)")

  process.trap_exits(False)
}

pub fn connect_properties_test() {
  process.trap_exits(True)
  process.flush_messages()

  // Connect with properties; make sure process exits normally.
  let assert Ok(client) =
    gemqtt.new(mqtt_server_host)
    |> gemqtt.set_port(mqtt_server_port)
    |> gemqtt.set_client_id("properties_test")
    |> gemqtt.set_property("Maximum-Packet-Size", 2048)
    |> gemqtt.set_property("User-Property", #("prop-name", "prop-value"))
    |> gemqtt.start_link

  let assert Ok(Nil) = gemqtt.connect(client)
  let assert Ok(_) = gemqtt.disconnect(client)
  should_exit_normally(client)

  process.trap_exits(False)
}

pub fn stop_client_test() {
  process.trap_exits(True)
  process.flush_messages()

  let client = new_test_client("stop_client_test")
  let assert Ok(Nil) = gemqtt.stop(client)
  should_exit_normally(client)

  process.trap_exits(False)
}

pub fn roundtrip_test() {
  let topic = "gemqtt/test/roundtrip"
  let msg_payload = bit_array.from_string("roundtrip payload")

  let client = new_test_client("roundtrip")
  let assert Ok(Nil) = gemqtt.connect(client)
  let assert Ok(_) = gemqtt.subscribe(client, topic)

  // Publish a test message.
  let assert Ok(_) =
    publisher.new(client, topic)
    |> publisher.publish(msg_payload)

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
  |> should.equal(msg_payload)

  got_msg.client
  |> should.equal(client)

  let assert Ok(_) = gemqtt.unsubscribe(client, [topic])
  let assert Ok(_) = gemqtt.disconnect(client)
}

// Creates and links a Client, configured for the test server.
// `connect` will still need to be called on the returned Client.
fn new_test_client(client_id: String) -> gemqtt.Client {
  let assert Ok(client) =
    gemqtt.new(mqtt_server_host)
    |> gemqtt.set_port(mqtt_server_port)
    |> gemqtt.set_client_id(client_id)
    |> gemqtt.set_connect_timeout(connect_timeout_seconds)
    |> gemqtt.set_owner(process.self())
    |> gemqtt.start_link

  client
}

// Verifies that the provided Client process exited normally, and returns
// the reason text. `process.trap_exits` must be True for this to succeed.
fn should_exit_abnormally(client: gemqtt.Client) -> String {
  let assert process.ExitMessage(pid: pid, reason: process.Abnormal(reason)) =
    process.new_selector()
    |> process.selecting_trapped_exits(identity)
    |> process.select_forever()

  // Verify exit message was from the process under test.
  client
  |> gemqtt.pid_of
  |> should.equal(pid)

  reason
}

// Verifies that the provided Client process exited normally.
// `process.trap_exits` must be True for this to succeed.
fn should_exit_normally(client: gemqtt.Client) -> Nil {
  let assert process.ExitMessage(pid: pid, reason: process.Normal) =
    process.new_selector()
    |> process.selecting_trapped_exits(identity)
    |> process.select_forever()

  // Verify exit message was from the process under test.
  client
  |> gemqtt.pid_of
  |> should.equal(pid)
}

// Maps an unexpected process message to `Result(t, String)`.
fn unexpected_message(msg) {
  Error("unexpected message: " <> string.inspect(msg))
}
