# cl-dotenv - Load .env files from Common Lisp
[![Build Status](https://travis-ci.org/ollelauribostrom/cl-dotenv.svg?branch=master)](https://travis-ci.org/ollelauribostrom/cl-dotenv) [![Coverage Status](https://coveralls.io/repos/github/ollelauribostrom/cl-dotenv/badge.svg?branch=master)](https://coveralls.io/github/ollelauribostrom/cl-dotenv?branch=master)

Tiny utility library for loading .env files.

> Currently tested with: SBCL, Allegro, ECL

## Usage
Calling `load-env` loads the environment from the specified .env file.

```lisp
  (.env:load-env (merge-pathnames "./path/.env"))
```

> NOTE: Available under the nicknames `.env` and `dotenv`

## Parsing rules
- Empty lines are skipped.
- Lines beginning with # are treated as comments
- Empty values are treated as empty strings, `EMPTY=` sets the environment variable EMPTY to `''`

> NOTE: Malformed or duplicated entries will trigger a restart to let you decide on how to proceed with the parsing.

## Installation
The package is not yet available through Quicklisp. ([Issue](https://github.com/quicklisp/quicklisp-projects/issues/1559))

**Manual installation steps**:

```bash
  cd ~/quicklisp/local-projects/   
  git clone https://github.com/ollelauribostrom/cl-dotenv.git
```

```lisp
  (ql:quickload :cl-dotenv)
```

## Dependencies
- [alexandria](https://common-lisp.net/project/alexandria/)
- [serapeum](https://github.com/ruricolist/serapeum)

## Contributing
All contributions are very much welcome. Please get familiar with the [contributing guide](https://github.com/ollelauribostrom/cl-dotenv/blob/master/.github/CONTRIBUTING.md).


## Commands
- `make install`: Install the packages **cl-dotenv** and **cl-dotenv-test** locally using [Roswell](https://github.com/roswell/roswell)   
- `make test`: Run the tests using [Prove](https://github.com/fukamachi/prove)
- `make coverage`: Run the tests and generate a coverage report
- `make sbcl`: Start SBCL and load **cl-dotenv**

## Author
* Olle Lauri Boström (ollebostr@gmail.com)

## Inspiration
- [dotenv](https://www.npmjs.com/package/dotenv)

## License
Licensed under the MIT License.
