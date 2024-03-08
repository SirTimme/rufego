package main;

type Consumer<Triangle<>,Square<>> struct {
    amount int
}

type Client struct {
    amount int
}

func (client Client) getValue() int {
    return client.amount
}

func (consumer Consumer<Triangle<>,Square<>>) getValue() int {
    return consumer.amount + Client{ 12 }.getValue()
}

func main() {
    _ = Consumer<Triangle<>,Square<>>{ 21 }.getValue()
}