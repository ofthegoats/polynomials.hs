# Polynomials.hs

Upon receiving a challenge to make a program which parses and solves a polynomial to an
arbitrary power, I made this.

Solution approximation uses
[Durand-Kerner](https://en.wikipedia.org/wiki/Durand%E2%80%93Kerner_method), solutions are
in the Cartesian form of complex numbers.

[Currently] Number of iterations of Durand-Kerner is hardcoded in `src/Solve.hs`

## Building

```shell
$ git clone https://github.com/ofthegoats/polynomials.hs.git
$ cd polynomials.hs
$ stack build
```

## Usage

```shell
$ stack run
```

Program takes input as a polynomial.

Terms in the polynomial are separate by a +/-/=.

There can only be one instance of =.

## License

Licensed under the BSD 3 clause license.

## Contributing

Format with Ormolu, avoid long lines, separate components into modules under `src/` as reasonable.
