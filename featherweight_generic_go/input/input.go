package main;

type Sender(type) interface {
    send(type)(a int) int
}

type Rectangle(type) struct {
    amount int
}

type Triangle(type) struct {}

func (this Rectangle(type)) send(type)(a int) int {
    return 1
}

type Receiver(type) interface {}

type Value(type T Sender(), V Receiver()) struct {
    sender T
    receiver T
}

type Box(type) struct {}

func main() {
    _ = Rectangle(){ 42 }.amount
}