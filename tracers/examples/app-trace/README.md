app-trace
=========

Test the `app-trace` idea.

Goal: solve adaptors blame problem.
- want to count each time imported variables are used
- counts should blame the use-site, not the contract-creation site

For data adaptors, this means that ALTHOUGH the contract appears between data &
data-adapted, the blame goes on the user / data-adapted boundary.


files
-----

- `data.rkt` define some untyped values
- `data-adapted.rkt` add types to the values in `data.rkt`
- `main.rkt` use values that came from the data adaptor

