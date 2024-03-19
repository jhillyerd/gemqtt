# gemqtt

[![Package Version](https://img.shields.io/hexpm/v/gemqtt)](https://hex.pm/packages/gemqtt)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/gemqtt/)

## NOTE

At the time of writing (March 19th, 2024), this package is stuck on emqtt
version 1.2 due to a [dependency
issue](https://github.com/jhillyerd/gemqtt/issues/21). That version of the
underlying [emqtt library](https://github.com/emqx/emqtt) only works with Erlang
OTP version 25.x, not 26+.

## Usage

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
