package main;

type Consumer struct {
    amount int
}

type Client struct {
    amount int
}

func (client Client) getValue() int {
    return client.amount
}

func (consumer Consumer) getValue() int {
    return consumer.amount + Client{ 12 }.getValue()
}

func main() {
    _ = Consumer{ 21 }.getValue()
}