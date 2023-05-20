# Kafka backend

This backend can be loaded as a plugin and then log items can be directed to a Kafka topic.

## Configuration options

 * "kafka_servers" - this is a string of a server name or IP address with port number separated by a ':'. Multiple servers can be indicated with separated names by ','.
 * "kafka_topic" - the name of the topic to which log items shall be sent to. Currently, only a single topic is supported.
 (place these key-value pairs in the configuration section "options")

## Example

the [example-complex](examples/complex/Main.lhs) is extended with sending out metrics to a Kafka topic.


## References

- using Haskell bindings to librdkafka: https://hackage.haskell.org/package/hw-kafka-client

- learn more about [Kafka](https://kafka.apache.org/)
