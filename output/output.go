package main;

type Top struct {}

func (this Rectangle<>) apply<16322240589235951052>() Top {
   return Top{}
}

type Rectangle<> struct {
}

func (this Increment<>) entry<>(function Function<Increment<>,Rectangle<>>) Any<> {
   return function.apply<>(this)
}

type Any<> interface {
}

func (this Increment<>) apply<8893616256338974947>() Top {
   return Top{}
}

func (this Increment<>) entry<3753352959197370218>() Top {
   return Top{}
}

type Increment<> struct {
}

func (this Increment<>) apply<>(incr Increment<>) Rectangle<> {
   return Rectangle<>{  }
}

type Function<Increment<>,Rectangle<>> interface {
   apply<>(param Increment<>) Rectangle<>
   apply<8893616256338974947>() Top
}

func main() {
   _ = Increment<>{  }.entry<>(Increment<>{  })
}
