package main;

type Any(type) interface {}

type Function(type A Any(), B Any()) interface {
    apply(type)(param A) B
}

type Increment(type) struct {}

type Rectangle(type) struct {
    amount int
}

func (this Rectangle(type)) otherFuncInterface(type)(function Function(Rectangle(), Increment())) Rectangle() {
    return function.apply()(this).toRectangle()()
}

func (this Rectangle(type)) apply(type)(input Rectangle()) Increment() {
    return Increment(){}
}

func (this Increment(type)) toRectangle(type)() Rectangle() {
    return Rectangle(){ 10 }
}

func (this Increment(type)) apply(type)(incr Increment()) Rectangle() {
    return Rectangle(){ 10 }.otherFuncInterface()(Rectangle(){ 10 })
}

func (this Increment(type)) entry(type)(function Function(Increment(), Rectangle())) Any() {
    return function.apply()(this)
}

func main() {
    _ = Increment(){}.entry()(Increment(){})
}
