package main;

type Sender(type) interface {
    send(type)(a Receiver()) int
}

type Receiver(type) interface {
    receive(type)(a int) int
}

type Rectangle(type) struct {
    amount int
}

type Triangle(type) struct {}

func (this Rectangle(type)) receive(type)(amount int) int {
    return this.amount
}

func (this Triangle(type)) send(type)(receiver Receiver()) int {
    return receiver.receive()(50)
}

func main() {
    _ = (Triangle(){}).send()(Rectangle(){ 42 })
}