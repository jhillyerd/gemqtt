import gemqtt
import gemqtt/publisher
import gemqtt/subscriber
import gleam/bit_array
import gleam/dynamic.{type Dynamic}
import gleam/erlang/atom
import gleam/erlang/process
import gleam/option.{None, Some}
import gleam/result
import gleeunit
import gleeunit/should
import helper

// MQTT host to test TLS options against.
const tls_test_host = "test.mosquitto.org"

// test.mosquitto.org self-signed certificate port.
const self_signed_port = 8883

// test.mosquitto.org lets-encrypt signed certificate port.
const letsencrypt_port = 8886

pub fn main() {
  gleeunit.main()
}

pub fn connect_test() {
  process.trap_exits(True)
  process.flush_messages()

  let client = helper.new_test_client("connect_test")
  let assert Ok(Nil) = gemqtt.connect(client)
  let assert Ok(_) = gemqtt.disconnect(client)
  helper.should_exit_normally(client)

  process.trap_exits(False)
}

// TODO add env var to enable this test.
// This TLS test depends on an external server and may be flaky.
pub fn valid_tls_connect_test() {
  process.trap_exits(True)
  process.flush_messages()

  // Test against self-signed cert, verification disabled.
  let assert Ok(client) =
    gemqtt.new(tls_test_host)
    |> gemqtt.set_port(self_signed_port)
    |> gemqtt.set_tls_enabled(True)
    |> gemqtt.set_tls_opt(
      "verify",
      dynamic.from(atom.create_from_string("verify_none")),
    )
    |> gemqtt.set_tls_opt("cacerts", cacerts_get())
    |> gemqtt.start_link()

  let assert Ok(Nil) = gemqtt.connect(client)
  let assert Ok(_) = gemqtt.disconnect(client)
  helper.should_exit_normally(client)

  // TODO experiment with SNI settings on newer emqtt/otp versions
  //
  // Test against CA signed cert, verification enabled.
  let assert Ok(client) =
    gemqtt.new(tls_test_host)
    |> gemqtt.set_port(letsencrypt_port)
    |> gemqtt.set_tls_enabled(True)
    |> gemqtt.set_tls_opt(
      "verify",
      dynamic.from(atom.create_from_string("verify_peer")),
    )
    |> gemqtt.set_tls_opt(
      "server_name_indication",
      dynamic.from(atom.create_from_string("disable")),
    )
    |> gemqtt.set_tls_opt("cacerts", cacerts_get())
    |> gemqtt.start_link()

  let assert Ok(Nil) = gemqtt.connect(client)
  let assert Ok(_) = gemqtt.disconnect(client)
  helper.should_exit_normally(client)

  process.trap_exits(False)
}

// TODO add env var to enable this test.
// This TLS test depends on an external server and may be flaky.
pub fn invalid_tls_connect_test() {
  process.trap_exits(True)
  process.flush_messages()

  let assert Ok(client) =
    gemqtt.new(tls_test_host)
    |> gemqtt.set_port(self_signed_port)
    |> gemqtt.set_tls_enabled(True)
    |> gemqtt.set_tls_opt(
      "verify",
      dynamic.from(atom.create_from_string("verify_peer")),
    )
    |> gemqtt.set_tls_opt("cacerts", cacerts_get())
    |> gemqtt.start_link()

  let assert Error(gemqtt.UnknownCa) = gemqtt.connect(client)
  helper.should_exit_abnormally(client)

  process.trap_exits(False)
}

pub fn server_name_test() {
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

pub fn connect_invalid_host_test() {
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

pub fn connect_invalid_port_test() {
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

pub fn connect_properties_test() {
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

pub fn bad_connect_properties_test() {
  // Try to start with invalid property name.
  let assert Error(got_err) =
    gemqtt.new(helper.mqtt_server_host)
    |> gemqtt.set_port(helper.mqtt_server_port)
    |> gemqtt.set_client_id("bad_properties_test")
    |> gemqtt.set_property("BAD_NAME", 2048)
    |> gemqtt.start_link

  let assert gemqtt.BadProperty(got_prop) = got_err
  got_prop
  |> should.equal(Some(atom.create_from_string("BAD_NAME")))

  // Try to start with invalid property value.
  let assert Error(got_err) =
    gemqtt.new(helper.mqtt_server_host)
    |> gemqtt.set_port(helper.mqtt_server_port)
    |> gemqtt.set_client_id("bad_properties_test")
    |> gemqtt.set_property("Maximum-Packet-Size", "smol")
    |> gemqtt.start_link

  let assert gemqtt.BadProperty(None) = got_err
}

pub fn disconnect_test() {
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

pub fn stop_test() {
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

// Returns the OS cacerts.
@external(erlang, "public_key", "cacerts_get")
fn cacerts_get() -> Dynamic
