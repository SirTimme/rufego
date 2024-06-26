# Rufego | Featherweight Go in Rust

Implementation of a Featherweight Go (FG) and Featherweight Generic Go (FGG) interpreter based on this paper:
https://arxiv.org/abs/2005.11710

## Featherweight Go

Run the FG interpreter. It interprets the input specified in the file "input.go" located under [featherweight_go/input/input.go](featherweight_go/input/input.go).

```
cargo run --bin featherweight_go
```

## Featherweight Generic Go

Run the FGG interpreter. It interprets the input specified in the file "input.go" located under [featherweight_generic_go/input/input.go](featherweight_generic_go/input/input.go).

```
cargo run --bin featherweight_generic_go
```

## Monomorphization

Run the monomorpher. It monomorphs the input specified in the file "input.go" located under [monomorpher/input/input.go](monomorpher/input/input.go) and saves the output under [monomorpher/output/output.go](monomorpher/output/output.go).

```
cargo run --bin monomorpher
```
