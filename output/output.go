package main;

type Top struct {}

func (this Increment<>) toRectangle<>() Rectangle<Increment<>> {
   return Rectangle<Increment<>>{ Increment<>{} }
}

func (this Rectangle<Increment<>>) otherFuncInterface<>(function Function<Rectangle<Increment<>>,Increment<>>) Rectangle<Increment<>> {
   return function.apply<>(this).toRectangle<>()
}

func (this Increment<>) apply<>(incr Increment<>) Rectangle<Increment<>> {
   return Rectangle<Increment<>>{ Increment<>{} }.otherFuncInterface<>(Rectangle<Increment<>>{ Increment<>{} })
}

func (this Increment<>) apply<8806198447648661727>() Top {
   return Top{}
}

func (this Increment<>) entry<12745335008652275624>() Top {
   return Top{}
}

func (this Increment<>) toRectangle<18044637097112106639>() Top {
   return Top{}
}

type Increment<> struct {}

func (this Rectangle<Increment<>>) apply<>(input Rectangle<Increment<>>) Increment<> {
   return Increment<>{}
}

func (this Increment<>) entry<>(function Function<Increment<>,Rectangle<Increment<>>>) Any<> {
   return function.apply<>(this)
}

type Any<> interface {}

func (this Rectangle<Increment<>>) otherFuncInterface<11132372325159962679>() Top {
   return Top{}
}

func (this Rectangle<Increment<>>) apply<5760591579707933380>() Top {
   return Top{}
}

type Rectangle<Increment<>> struct {
   amount Increment<>
}

type Function<Rectangle<Increment<>>,Increment<>> interface {
   apply<>(param Rectangle<Increment<>>) Increment<>
   apply<5760591579707933380>() Top
}

type Function<Increment<>,Rectangle<Increment<>>> interface {
   apply<>(param Increment<>) Rectangle<Increment<>>
   apply<8806198447648661727>() Top
}

func main() {
   _ = Increment<>{}.entry<>(Increment<>{})
}
