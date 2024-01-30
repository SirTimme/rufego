package main;

type Sender(type) interface {
    send(type)(a int) int
}

type Rectangle(type) struct {
    value int
    amount int
}

type Triangle(type) struct {}

func (this Rectangle(type)) send(type)(a int) int {
    return 1
}

type Receiver(type) interface {}

type Value(type T Sender(), V Receiver()) struct {
    sender T
    receiver V
}

type Box(type) struct {}

func main() {
    _ = Value(Rectangle(), Box()){ Rectangle(){ 4, 2 }, Triangle(){} }
}