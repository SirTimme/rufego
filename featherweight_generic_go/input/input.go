package main;

type Any(type) interface {}

type Rectangle(type V Any(), R Any()) struct {
    amount V
    client R
}

type Triangle(type T Any()) struct {
    value T
}

type Flight(type) struct {
    amount int
}

type Box(type) struct {}

type Square(type) struct {}

func (this Flight(type)) send(type T Any())(param T) Any() {
    return Triangle(T){ param }
}

func main() {
    _ = Flight(){ 69 }.send(Square())(Square(){})
}