# cl-dotenv - Load .env files from Common Lisp
[![Build Status](https://travis-ci.org/ollelauribostrom/cl-dotenv.svg?branch=master)](https://travis-ci.org/ollelauribostrom/cl-dotenv) [![Coverage Status](https://coveralls.io/repos/github/ollelauribostrom/cl-dotenv/badge.svg?branch=master)](https://coveralls.io/github/ollelauribostrom/cl-dotenv?branch=master)

Tiny utility library for loading .env files & getting/setting environment variables.

> Currently tested with: SBCL, Allegro, ECL, CLISP

## Usage
> NOTE: Corrupt .env files will generate an error when being loaded. Make sure your .env file is in the format:   
```bash
  key1=value   
  key2=value   
```

### Load & set the environment from an .env file
Calling `load-env` loads the environment from the specified .env file.
```lisp
  (dotenv:load-env (merge-pathnames "./path/.env")) 
    => t
```

### Get an environment variable    
The value of a variable can be read by calling `get-env` with the name of the variable.
```lisp
  (dotenv:get-env "SOME_VAR")
    => "value" or nil
```
If you wish, you can provide a default value that is returned if the variable you are trying to read is not set. 
```lisp
  (dotenv:get-env "SOME_NONEXISTING_VAR" "default-value")
    => "default-value"
```

### Set an environment variable   
Setting a variable overwrites any existing value   
```lisp
  (dotenv:set-env "SOME_VAR" "some_value")
    => t
```

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
- [The Common Lisp Cookbook, Interfacing with your OS](http://cl-cookbook.sourceforge.net/os.html)
- [LispForum, Setting Path Variable](http://www.lispforum.com/viewtopic.php?f=2&t=446)
- [dotenv](https://www.npmjs.com/package/dotenv)

## License
Licensed under the MIT License.
