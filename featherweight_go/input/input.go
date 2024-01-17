package main;

type Container interface {
    createConsumer(self Container) int
    getValue() int
}

type Consumer struct {
    amount int
}

func (c Consumer) createConsumer(self Container) int {
    return self.getValue()
}

func (c Consumer) getValue() int {
    return c.amount
}

func main() {
    _ = (Consumer{ 0 }.createConsumer(Consumer{ (2 + 5) * 3 })) + (Consumer{ 21 }.getValue())
}