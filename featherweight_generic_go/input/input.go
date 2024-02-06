package main;

type Any(type) interface {}

type Box(type) struct {}

type Square(type) struct {}

type Rectangle(type T Any(), R Any()) struct {
    sender T
    receiver R
}

func main() {
    _ = Rectangle(Box(), Square()){ Box(){}, Square(){} }
}