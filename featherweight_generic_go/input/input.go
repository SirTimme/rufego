package main;

type Sender(type) interface {
    send(type)(a Receiver()) int
}

type Any(type) interface {}

type Receiver(type) interface {
    receive(type)(a int) int
}

type Rectangle(type) struct {
    amount int
}

type Triangle(type) struct {}

func (this Rectangle(type)) getAmount(type T Any())() int {
    return this.amount
}

func (this Triangle(type)) buildRectangle(type T Any())() T {
    return Rectangle(){ 69 }
}

func main() {
    _ = Triangle(){}.buildRectangle(Rectangle())().getAmount(Triangle())()
}