# gemqtt

[![Package Version](https://img.shields.io/hexpm/v/gemqtt)](https://hex.pm/packages/gemqtt)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/gemqtt/)

gemqtt provides an MQTT client for Gleam projects running on the BEAM.  It does
so by wrapping the [emqx/emqtt] library written in Erlang.

## NOTE

At the time of writing (March 19th, 2024), this package is stuck on emqtt
version 1.2 due to a [dependency
issue](https://github.com/jhillyerd/gemqtt/issues/21). That version of the
underlying [emqx/emqtt] library only works with Erlang OTP version 25.x, not
26+.

## Status

This package is a work in progress. If it is missing an emqtt feature you need,
please send a PR.

### General features

- [x] Connect to unencrypted MQTT servers over TCP
- [x] Connect to TLS encrypted MQTT servers over TCP
- [ ] Connect to unencrypted MQTT servers over websocket
- [ ] Connect to TLS encrypted MQTT servers over websocket
- [x] Supports [emqtt properties] for connect, publish, and subscribe

### Connect options

- [x] User + password authentication
- [x] Message auto acknowledgement
- [x] Clean start
- [x] Client ID
- [x] Connect timeout
- [x] Erlang server name
- [x] Owner PID (for disconnect notifications)
- [x] Port number
- [x] TLS enable + raw options
- [ ] TCP options
- [ ] Websocket path
- [ ] Bridge mode
- [ ] Proto version
- [ ] Keep alive
- [ ] Max in-flight
- [ ] Retry interval
- [ ] Will topic
- [ ] Will payload
- [ ] Will retain
- [ ] Will QoS
- [ ] Will properties
- [ ] Ack timeout
- [ ] Force ping

### Publish options

- [x] QoS
- [x] Retain

### Subscribe options

- [x] Local Echo
- [x] QoS
- [x] Retain as published
- [x] Retain handling

## Example Usage

```sh
gleam add gleam_erlang
gleam add gemqtt
```
```gleam
import gemqtt
import gemqtt/publisher
import gemqtt/subscriber
import gleam/bit_array
import gleam/erlang/process
import gleam/io

pub fn main() {
  let topic = "gemqtt/readme/example"

  // Create a client and connect to the test server.
  let assert Ok(client) =
    gemqtt.new("test.mosquitto.org")
    |> gemqtt.start_link
  let assert Ok(_) = gemqtt.connect(client)

  io.println("Connected.")

  // Subscribe to messages from the topic.
  let assert Ok(_) =
    client
    |> subscriber.new
    |> subscriber.add(topics: [topic])

  io.println("Subscribed.")

  // Publish a test message to the topic.
  let assert Ok(_) =
    publisher.new(client, topic)
    |> publisher.publish(bit_array.from_string("Hello from Gleam!"))

  io.println("Sent message.")

  // Attempt to receive a message from the topic.
  let assert Ok(got_msg) =
    process.new_selector()
    |> subscriber.selecting_mqtt_messages(Ok)
    |> process.select_forever

  io.println("Received message:")
  io.debug(got_msg)

  let assert Ok(_) = subscriber.remove(client, [topic])
  let assert Ok(_) = gemqtt.disconnect(client)
}
```

Further documentation can be found at <https://hexdocs.pm/gemqtt>.

## Development

```sh
gleam test  # Run the tests
gleam shell # Run an Erlang shell
```


[emqx/emqtt]:       https://github.com/emqx/emqtt
[emqtt properties]: https://github.com/emqx/emqtt?tab=readme-ov-file#properties
