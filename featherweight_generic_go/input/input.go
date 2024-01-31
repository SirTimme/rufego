package main;

type Sender(type) interface {
    send(type)(a int) int
}

type Receiver(type) interface {}

type Rectangle(type) struct {
    amount int
}

type Triangle(type) struct {
    value int
}

type Value(type T Sender(), V Receiver()) struct {
    sender T
    receiver Rectangle()
}

func (this Rectangle(type)) send(type)(a int) int {
    return this.amount
}

func main() {
    _ = ((Value(Rectangle(), Triangle()){ Rectangle(){ 10 }, Rectangle(){ 42 } }).receiver).send()(10)
}