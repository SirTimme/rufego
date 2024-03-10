package main;

type Any(type) interface {}

type Function(type A Any(), B Any()) interface {
    apply(type)(param A) B
}

type Increment(type) struct {}

type Rectangle(type) struct {}

func (this Rectangle(type)) apply(type)(rect Rectangle()) Rectangle() {
    return this
}

func (this Increment(type)) apply(type)(incr Increment()) Rectangle() {
    return Rectangle(){}
}

func (this Increment(type)) entry(type)(function Function(Increment(), Rectangle())) Any() {
    return function.apply()(this)
}

func main() {
    _ = Increment(){}.entry()(Increment(){})
}