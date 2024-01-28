package main;

type Container(type T Client(int)) interface {}

type Client(type) interface {}

type Box(type) struct {
    value int
    b int
    c int
}

type Consumer(type T Container(Client(int))) struct {
    a Consumer(Container())
    b T
}

func (this Consumer(type T Client(T))) build(type)(client T) T {
    return client
}

func main() {
    _ = (Box(){ 1, 5, 6 }.b)
}