package main;

type Top struct {}

func (this Rectangle<>) otherFuncInterface<>(function Function<Rectangle<>,Increment<>>) Rectangle<> {
   return function.apply<>(this).toRectangle<>()
}

type Any<> interface {}

type Function<Rectangle<>,Increment<>> interface {
   apply<>(param Rectangle<>) Increment<>
   apply<12410948882031173846>() Top
}

func (this Increment<>) toRectangle<>() Rectangle<> {
   return Rectangle<>{ 10 }
}

func (this Increment<>) entry<>(function Function<Increment<>,Rectangle<>>) Any<> {
   return function.apply<>(this)
}

func (this Increment<>) apply<>(incr Increment<>) Rectangle<> {
   return Rectangle<>{ 10 }.otherFuncInterface<>(Rectangle<>{ 10 })
}

type Function<Increment<>,Rectangle<>> interface {
   apply<>(param Increment<>) Rectangle<>
   apply<8893616256338974947>() Top
}

func (this Rectangle<>) apply<12410948882031173846>() Top {
   return Top{}
}

func (this Rectangle<>) otherFuncInterface<13284963735046141154>() Top {
   return Top{}
}

type Rectangle<> struct {
   amount int
}

func (this Rectangle<>) apply<>(input Rectangle<>) Increment<> {
   return Increment<>{}
}

func (this Increment<>) apply<8893616256338974947>() Top {
   return Top{}
}

func (this Increment<>) toRectangle<14099019175152719434>() Top {
   return Top{}
}

func (this Increment<>) entry<3753352959197370218>() Top {
   return Top{}
}

type Increment<> struct {}

func main() {
   _ = Increment<>{}.entry<>(Increment<>{})
}
