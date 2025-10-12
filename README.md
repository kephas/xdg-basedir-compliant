# XDG Basedir implementation

This is a compliant implementation of the [XDG Base Directory Specification](https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html). In all environment variabes, it treats relative paths as invalid.

## `XDG_DATA_HOME`/`XDG_DATA_DIRS` and `XDG_CONFIG_HOME`/`XDG_CONFIG_DIRS`

With respect to data and config directories, this implementation can:
- get the home path
- get the paths list
- read the first readable file
- merge all readable files
- write the file in the home path

## `XDG_CACHE_HOME`, `XDG_STATE_HOME` and `XDG_RUNTIME_DIR`

With respect to cache, state and runtime, this implementation can:
- get the single path
- read the file
- write the file

As this library deals purely with accessing files, I eventually chose not to implement the notion of warning the user when `XDG_RUNTIME_DIR` isn't set and of providing a replacement directory. Any application where that behaviour would be relevant can use the exception raised by `getRuntimeDir` to detect this problem, should deal with warning the user in its own context, and can set `XDG_RUNTIME_DIR` with a path that makes sense for that application.

# Build

You can build the project with:

```
cabal build
```

If you have Nix installed, `default.nix` provides an environment with all the needed dependencies. It is suitable for `nix-direnv`. You can also build the project with:

```
nix-build .
```
