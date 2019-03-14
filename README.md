# parse_transform_compiled_regex

Erlang parse_transform to have regex compiled on build time

## How to use:

Include it as a dependency and add the `parse_transform` compile option to each
module or globally:

```
-compile({parse_transform, parse_transform_compiled_regex}).
```
The calls to `re:compile`, `re:replace`, `re:run` and `re:split` that have the
regex and options directly available in the call have the regex precompiled.  
E.g.:
```
    _ = re:compile(?HTTP_REGEX), %% Transformed

    _ = re:compile([$a | ?HTTP_REGEX]), %% Transformed

    _ = re:compile(give_me_the_regex()), %% Not transformed

    HttpRegex = ?HTTP_REGEX,
    _ = re:compile(HttpRegex), %% Not transformed

    _ = re:compile(?HTTP_REGEX, [caseless]), %% Transformed

    Options = [caseless],
    _ = re:compile(?HTTP_REGEX, Options), %% Not transformed
```

## Results:

From the `timed` test included in this project

```
re:run time:
Precompiled (10000 iterations): 30619
Repeated compile (10000 Iterations): 50553

re:compile time:
Precompiled (10000 iterations): 433
Repeated compile (10000 Iterations): 22052
```
(re:compile with the precompiled regex is a noop)

## Run tests:
```
rebar3 ct
```

## Authors

* **Jose M Perez Ramos** - [Kuroneer](https://github.com/Kuroneer)

## License

[MIT License](LICENSE)

