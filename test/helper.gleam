import gemqtt
import gemqtt/subscriber
import gleam/dynamic.{type Dynamic}
import gleam/erlang/process
import gleam/function.{identity}
import gleam/io
import gleam/string
import gleam/result
import gleeunit/should

/// External test MQTT server host.  Must accept anonymous connections.
pub const mqtt_server_host = "localhost"

/// External test MQTT server port.
pub const mqtt_server_port = 41_883

/// Timeout when initially connecting to the MQTT server.
pub const connect_timeout_seconds = 2

/// Timeout waiting for a message to be received from the MQTT server.
pub const recv_timeout_millis = 2000

pub fn debug(term: t) -> t {
  io.println(string.inspect(term))
  term
}

/// Creates and links a Client, configured for the test server.
/// `connect` will still need to be called on the returned Client.
pub fn new_test_client(client_id: String) -> gemqtt.Client {
  let assert Ok(client) =
    gemqtt.new(mqtt_server_host)
    |> gemqtt.set_port(mqtt_server_port)
    |> gemqtt.set_client_id(client_id)
    |> gemqtt.set_connect_timeout(connect_timeout_seconds)
    |> gemqtt.set_owner(process.self())
    |> gemqtt.start_link

  client
}

/// Verifies that the provided Client process exited normally, and returns
/// the reason text. `process.trap_exits` must be True for this to succeed.
pub fn should_exit_abnormally(client: gemqtt.Client) -> String {
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

/// Verifies that the provided Client process exited normally.
/// `process.trap_exits` must be True for this to succeed.
pub fn should_exit_normally(client: gemqtt.Client) -> Nil {
  let assert process.ExitMessage(pid: pid, reason: process.Normal) =
    process.new_selector()
    |> process.selecting_trapped_exits(identity)
    |> process.select_forever()

  // Verify exit message was from the process under test.
  client
  |> gemqtt.pid_of
  |> should.equal(pid)
}

type MqttMsg {
  Message(subscriber.Message)
  PubAck(subscriber.PubAck)
  Other(Dynamic)
}

/// Select next MQTT message from mailbox, or fail.
pub fn get_message() -> subscriber.Message {
  let assert Ok(msg) =
    process.new_selector()
    |> subscriber.selecting_mqtt_messages(Message)
    |> subscriber.selecting_mqtt_pubacks(PubAck)
    |> process.selecting_anything(Other)
    |> process.select(within: recv_timeout_millis)
    |> result.replace_error("timeout waiting for message")

  case msg {
    Message(message) -> message
    PubAck(_) -> get_message()
    Other(msg) -> {
      Error(#("unexpected message", msg))
      |> should.be_ok()
    }
  }
}

/// Maps an unexpected process message to `Result(t, String)`.
pub fn unexpected_message(msg) {
  Error("unexpected message: " <> string.inspect(msg))
}
