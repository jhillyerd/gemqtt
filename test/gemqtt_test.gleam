import gemqtt
import gemqtt/publisher
import gemqtt/subscriber
import gleam/bit_array
import gleam/erlang/process
import gleam/result
import gleeunit
import gleeunit/should
import helper

pub fn main() {
  gleeunit.main()
}

pub fn client_connect_test() {
  process.trap_exits(True)
  process.flush_messages()

  let client = helper.new_test_client("connect_test")
  let assert Ok(Nil) = gemqtt.connect(client)
  let assert Ok(_) = gemqtt.disconnect(client)
  helper.should_exit_normally(client)

  process.trap_exits(False)
}

pub fn client_server_name_test() {
  process.trap_exits(False)

  let assert Ok(client) =
    gemqtt.new(helper.mqtt_server_host)
    |> gemqtt.set_port(helper.mqtt_server_port)
    |> gemqtt.set_connect_timeout(helper.connect_timeout_seconds)
    |> gemqtt.set_name("gemqtt_dup")
    |> gemqtt.start_link

  // Start another Client with the same Erlang server name.
  let assert Error(gemqtt.AlreadyStarted(orig_pid)) =
    gemqtt.new(helper.mqtt_server_host)
    |> gemqtt.set_port(helper.mqtt_server_port)
    |> gemqtt.set_connect_timeout(helper.connect_timeout_seconds)
    |> gemqtt.set_name("gemqtt_dup")
    |> gemqtt.start_link

  orig_pid
  |> should.equal(gemqtt.pid_of(client))

  let assert Ok(Nil) = gemqtt.stop(client)
}

pub fn client_connect_invalid_host_test() {
  process.trap_exits(True)
  process.flush_messages()

  let assert Ok(client) =
    gemqtt.new("invalid.example.com")
    |> gemqtt.set_port(helper.mqtt_server_port)
    |> gemqtt.set_connect_timeout(helper.connect_timeout_seconds)
    |> gemqtt.start_link

  let assert Error(gemqtt.Nxdomain) = gemqtt.connect(client)

  client
  |> helper.should_exit_abnormally
  |> should.equal("Shutdown(Nxdomain)")

  process.trap_exits(False)
}

pub fn client_connect_invalid_port_test() {
  process.trap_exits(True)
  process.flush_messages()

  let assert Ok(client) =
    gemqtt.new(helper.mqtt_server_host)
    |> gemqtt.set_port(41_999)
    |> gemqtt.set_connect_timeout(helper.connect_timeout_seconds)
    |> gemqtt.start_link

  let assert Error(gemqtt.Econnrefused) = gemqtt.connect(client)

  client
  |> helper.should_exit_abnormally
  |> should.equal("Shutdown(Econnrefused)")

  process.trap_exits(False)
}

pub fn client_connect_properties_test() {
  process.trap_exits(True)
  process.flush_messages()

  // Connect with properties; make sure process exits normally.
  let assert Ok(client) =
    gemqtt.new(helper.mqtt_server_host)
    |> gemqtt.set_port(helper.mqtt_server_port)
    |> gemqtt.set_client_id("properties_test")
    |> gemqtt.set_property("Maximum-Packet-Size", 2048)
    |> gemqtt.set_property("User-Property", #("prop-name", "prop-value"))
    |> gemqtt.start_link

  let assert Ok(Nil) = gemqtt.connect(client)
  let assert Ok(_) = gemqtt.disconnect(client)
  helper.should_exit_normally(client)

  process.trap_exits(False)
}

pub fn client_disconnect_test() {
  process.trap_exits(True)
  process.flush_messages()

  let client = helper.new_test_client("disconnect_client_test")
  let assert Ok(Nil) = gemqtt.connect(client)
  let assert Ok(Nil) = gemqtt.disconnect(client)
  helper.should_exit_normally(client)

  // Verify noproc exception is caught and returned.
  let assert Error(gemqtt.Noproc) = gemqtt.disconnect(client)

  process.trap_exits(False)
}

pub fn client_stop_test() {
  process.trap_exits(True)
  process.flush_messages()

  let client = helper.new_test_client("stop_client_test")
  let assert Ok(Nil) = gemqtt.stop(client)
  helper.should_exit_normally(client)

  // Verify noproc exception is caught and returned.
  let assert Error(gemqtt.Noproc) = gemqtt.stop(client)

  process.trap_exits(False)
}

pub fn roundtrip_test() {
  process.trap_exits(False)
  process.flush_messages()

  let topic = "gemqtt/test/roundtrip"
  let msg_payload = bit_array.from_string("roundtrip payload")

  let client = helper.new_test_client("roundtrip_test")
  let assert Ok(Nil) = gemqtt.connect(client)
  let assert Ok(_) =
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

  got_msg.payload
  |> should.equal(msg_payload)

  let assert Ok(_) = subscriber.remove(client, [topic])
  let assert Ok(_) = gemqtt.disconnect(client)
}
