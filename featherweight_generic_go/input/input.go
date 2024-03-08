package main;

type Any(type) interface {
    send(type)() int
    receive(type)() Box()
}

type Any2(type) interface {}

type Rectangle(type V Any(), R Any()) struct {
    amount V
    client R
}

type Triangle(type T Any()) struct {
    value T
}

func (this Triangle(type T Any())) send(type)() int {
    return 0
}

type Flight(type T Any(), V T) struct {
    amount T
    amno V
}

type Box(type) struct {
    amount int
}

func (this Box(type)) send(type)() int {
    return 10
}

func (this Box(type)) receive(type)() Box() {
    return this
}

func (this Box(type)) dummy(type R Any())(param R) R {
    return param
}

type Square(type) struct {}

func (this Flight(type T Any(), V T)) send(type)(param Any()) int {
    return param.send()()
}

func main() {
    _ = Flight(Box(), Box()){ Box(){ 5 }, Box(){ 5 } }.send()(Box(){ 42 }) + Box(){ 5 }.dummy(Box())(Box(){ 10 }).amount
}