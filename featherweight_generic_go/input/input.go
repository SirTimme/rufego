package main;

type Container(type T Client()) interface {}

type Client(type V Client()) interface {
    build(type R V)(a R, b V) int
    destroy(type R V)() int
}

type Box(type V Client, R V) struct {
    value int
    b int
    c int
}

type Consumer(type T Container(Client(int))) struct {
    a Consumer(Container())
    b T
}

func (this Consumer(type T Client())) build(type V T)(client T) V {
    return client
}

func main() {
    _ = (Box(){ 1, 5, 6 }.b)
}