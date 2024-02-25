package main;

type Any(type) interface {}

type Rectangle(type V Any(), R Any()) struct {
    amount V
    client R
}

type Triangle(type T Any()) struct {
    value Flight()
}

type Flight(type) struct {
    amount int
}

func (this Flight(type)) send(type)() Flight() {
    return Triangle(Flight()){ Flight() { 42 } }
}

func main() {
    _ = Rectangle(Flight(), Triangle(Flight())){ Flight(){ 42 }, Triangle(Flight()){ Flight(){ 69 } } }.client.value.send()()
}