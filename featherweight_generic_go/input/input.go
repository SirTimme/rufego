package main;

type All(type) interface {}

type Any(type T All()) interface {
    send(type)() T
}

type Rectangle(type V All(), R V) struct {
    amount V
    client R
}

type Triangle(type R Any(All())) struct {}

type Box(type) struct {}

func (this Triangle(type R Any(All()))) send(type T All())(param T) T {
    return param
}

func (this Rectangle(type V All(), R V)) send(type T All())(param T) T {
    return param
}

func (this Rectangle(type V All(), R V)) test(type)(amount R) R {
    return amount
}

func (this Box(type)) send(type)() int {
    return 69
}

func main() {
    _ = Rectangle(Box(), Box()){ Box(){}, Box(){} }.test()(Box(){})
}