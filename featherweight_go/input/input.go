package main;

type Consumer<Triangle> struct {
    amount int
}

type Client struct {
    amount int
}

func (client Client) getValue() int {
    return client.amount
}

func (consumer Consumer<Triangle>) getValue() int {
    return consumer.amount + Client{ 12 }.getValue()
}

func main() {
    _ = Consumer<Triangle>{ 21 }.getValue()
}