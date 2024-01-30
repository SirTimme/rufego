package main;

type Sender(type) interface {
    send(type)(a int) int
}

type Rectangle(type) struct {
    amount int
}

type Triangle(type) struct {
    value int
}

func (this Rectangle(type)) send(type)(a int) int {
    return this.amount
}

func (this Value(type T Sender(), V Receiver())) send(type)(a int) int {
    return 10
}

type Receiver(type) interface {}

type Value(type T Sender(), V Receiver()) struct {
    sender T
    receiver V
}

type Box(type) struct {}

func main() {
    _ = (Value(Rectangle(), Triangle()){ Rectangle(){ 42 }, Triangle(){ 42 } }).receiver
}