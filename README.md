# cl-dotenv - Load .env files from Common Lisp
Tiny utility library for loading .env files & getting/setting environment variables.

## Usage
Corrupt .env files will generate an error when being loaded. Make sure your .env file is in the format:   
```bash
  key1=value   
  key2=value   
```

Load & set environment from .env file   
```lisp
  (dotenv:load-env (merge-pathnames "./path/.env")) 
    => t
```

Get environment variable     
```lisp
  (dotenv:get-env "SOME_VAR")
    => "value" or nil
```

Get environment variable and return default value if not set   
```lisp
  (dotenv:get-env "SOME_NONEXISTING_VAR" "default-value")
    => "default-value"
```

Set environment variable   
> Setting a environment variable overwrites any existing value   
```lisp
  (dotenv:set-env "SOME_VAR" "some_value")
    => t
```

## Installation
The package is not yet available through Quicklisp.

**Manual installation steps**:
```bash
  cd ~/quicklisp/local-projects/   
  git clone https://github.com/ollelauribostrom/cl-dotenv.git  
```
```lisp
  (ql:quickload :cl-dotenv)
```

## Commands
- `make test`: Run all tests using SBCL   
- `make load`: Start SBCL and load library

## Author
* Olle Lauri Boström (ollebostr@gmail.com)

## Inspiration
- [The Common Lisp Cookbook, Interfacing with your OS](http://cl-cookbook.sourceforge.net/os.html)
- [LispForum, Setting Path Variable](http://www.lispforum.com/viewtopic.php?f=2&t=446)

## License
Licensed under the MIT License.
