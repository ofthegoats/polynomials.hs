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

## Example

```shell
$ stack run
x^3 + 2xx + 9 = -11
1.0x^3.0 + 2.0x^2.0 + 20.0 = 0
x_1 ≈ 0.7848018266102612 - 2.23314778453945i
x_2 ≈ 0.7848018266102613 + 2.23314778453945i
x_3 ≈ -3.5696036532205233 + 1.3602980171576907e-16i
```

Note that as the method used is an approximation, complex solutions whose imaginary
component is very small (e.g. `x_3` in above example) may not actually have any imaginary
component.

As it is technically possible for a solution to just have a very small imaginary
component, I do not intend to change this, though I may implement some other check at
another time.

For now, just observe is there is any other solution which is the complex conjugate of the
suspicious solution. If there is not, it _must_ be a real solution (for any polynomial
there is an even number of solutions with a nonzero imaginary component).

## License

Licensed under the BSD 3 clause license.

## Contributing

Format with Ormolu, avoid long lines, separate components into modules under `src/` as reasonable.

## TODO

- [ ] implement tests
  - [ ] parsing
  - [ ] solving (within a reasonable margin of error)
