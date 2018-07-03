## 0.5.0 (2017-05-25)
* Split into 2 toplevel packages: ezjsonm and ezjsonm-lwt
* Build with jbuilder
* Fix error messages to be more specific when failing to parse an int
* Support `sexp_of_t` `t_of_sexp` `sexp_of_value` `value_of_sexp`

## 0.4.3 (2015-11-30)
* Fix support for OCaml 4.03 (@samoht)

## 0.4.2 (2015-08-26)
* Add unit-tests (#2)
* Fix the parsing of JSON streams. Normal JSON arrays should be valid
  streams (#16)

## 0.4.1 (2014-02-02)
* Use polymorphic variants subtyping to avoid manual coercion in the
  API (#11, patch from Julien Sagot)

## 0.4.0 (2014-12-17)
* Clean-up the typed representation of serializable JSON
    (#5, report and patch from Rudi Grinberg)
* add int32/int64/triple combinators
* fix a bug with the option types
* fix the type of the `unit` combinator

## 0.3.1 (2014-11-20)
* Expose [parse_error]

## 0.3.0 (2014-10-24)
* Add sexpilb conversion functions
* Add functions to encode/decode non utf8 strings (using hex encoding)

## 0.2.0 (2012-12-13)
* Add a new module `Ezjsonm_lwt`, to convert string streams to json streams
* Expose `Ezjsonm.get_bool`

## 0.1.0 (2012-12-12):
* Initial version
