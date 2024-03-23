package main;

type Any(type) interface {}

type Function(type A Any(), B Any()) interface {
    apply(type)(param A) B
}

type Increment(type) struct {}

type Rectangle(type T Any()) struct {
    amount T
}

func (this Rectangle(type T Any())) otherFuncInterface(type)(function Function(Rectangle(Increment()), Increment())) Rectangle(Increment()) {
    return function.apply()(this).toRectangle()()
}

func (this Rectangle(type T Any())) apply(type)(input Rectangle(Increment())) Increment() {
    return Increment(){}
}

func (this Increment(type)) toRectangle(type)() Rectangle(Increment()) {
    return Rectangle(Increment()){ Increment(){} }
}

func (this Increment(type)) apply(type)(incr Increment()) Rectangle(Increment()) {
    return Rectangle(Increment()){ Increment(){} }.otherFuncInterface()(Rectangle(Increment()){ Increment(){} })
}

func (this Increment(type)) entry(type)(function Function(Increment(), Rectangle(Increment()))) Any() {
    return function.apply()(this)
}

func main() {
    _ = Increment(){}.entry()(Increment(){})
}
