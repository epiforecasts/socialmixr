# Handle deprecated argument

Handle deprecated argument

## Usage

``` r
deprecate_arg(old_arg, new_arg, old_name, new_name, fn_name, version = "0.5.0")
```

## Arguments

- old_arg:

  the deprecated argument value

- new_arg:

  the new argument value

- old_name:

  the old argument name (with dot)

- new_name:

  the new argument name (with underscore)

- fn_name:

  the function name

- version:

  the version when deprecated

## Value

`new_arg`. Aborts via
[`lifecycle::deprecate_stop()`](https://lifecycle.r-lib.org/reference/deprecate_soft.html)
if `old_arg` is supplied.
