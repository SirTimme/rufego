package main;

type Top struct {}

func (this Flight<Box<>,Box<>>) send<>(param Any<>) int {
   return param.send<>()
}

func (this Square<>) send<>() int {
   return 1
}

func (this Flight<Box<>,Box<>>) send<17150361252704764870>() Top {
   return Top{}
}

type Flight<Box<>,Box<>> struct {
   amount Box<>
   amno Box<>
}

func (this Box<>) send<17959394416299051234>() Top {
   return Top{}
}

func (this Box<>) receive<9917919832045682780>() Top {
   return Top{}
}

func (this Box<>) dummy<17768842318798519907>() Top {
   return Top{}
}

type Box<> struct {
   amount int
}

func (this Square<>) send<17959394416299051234>() Top {
   return Top{}
}

func (this Square<>) receive<9917919832045682780>() Top {
   return Top{}
}

type Square<> struct {
   content int
}

func (this Box<>) dummy<Box<>>(param Box<>) Box<> {
   return param
}

func (this Box<>) send<>() int {
   return this.dummy<Square<>>(Square<>{ 10 }).content
}

func (this Box<>) dummy<Square<>>(param Square<>) Square<> {
   return param
}

type Any<> interface {
   send<>() int
   send<17959394416299051234>() Top
   receive<9917919832045682780>() Top
}

func main() {
   _ = Flight<Box<>,Box<>>{ Box<>{ 5 }, Box<>{ 5 } }.send<>(Box<>{ 42 }) + Box<>{ 5 }.dummy<Box<>>(Box<>{ 10 }).amount
}
