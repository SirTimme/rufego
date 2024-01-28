package main;

type Container(type T Client(int)) interface {}

type Client(type) interface {}

type Consumer(type T Container(Client(int))) struct {
    a Consumer(Container())
    b T
}

func (this Consumer(type T Client(T))) build(type)(client T) T {
    return client
}

func main() {
    _ = 1 + 1
}