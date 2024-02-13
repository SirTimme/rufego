package main;

type Any(type) interface {
    send(type)() int
}

type Rectangle(type) struct {
    amount int
}

type Triangle(type) struct {}

func (this Rectangle(type)) send(type)() int {
    return this.amount
}

func (this Triangle(type)) send(type)() int {
    return 1 + 1
}

func (this Triangle(type)) buildRectangle(type T Any())(param T) int {
    return param.send()()
}

func main() {
    _ = Triangle(){}.buildRectangle(Triangle())(Triangle(){ 15 })
}