package main;

type Container(type) interface {}

type Client(type) struct {}

type Consumer(type T Client()) struct {
    a Consumer(int)
    b T
}

func (this Consumer(type T Client(T), V Consumer(int))) build(type)(a int, client T) T {
    return 0
}

func main() {
    _ = 1
}