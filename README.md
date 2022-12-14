# XDG Basedir implementation

This is a compliant implementation of the [XDG Base Directory Specification](https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html).

## Compliance

- [X] `$XDG_DATA_HOME`
  - [X] get path
  - [X] read file
  - [X] write file
- [X] `$XDG_CONFIG_HOME`
  - [X] get path
  - [X] read file
  - [X] write file
- [X] `$XDG_STATE_HOME`
  - [X] get path
  - [X] read file
  - [X] write file
- [X] `$XDG_DATA_DIRS`
  - [X] get path list
  - [X] read best file
  - [X] merge files
- [X] `$XDG_CONFIG_DIRS`
  - [X] get path list
  - [X] read best file
  - [X] merge files
- [X] `$XDG_CACHE_HOME`
  - [X] get path
  - [X] read file
  - [X] write file
- [X] `$XDG_RUNTIME_DIR`
  - [X] get path
  - [X] read file
  - [X] write file
- [X] treat relative paths as invalid

As this library deals purely with accessing files, I eventually chose not to implement the notion of warning the user when `$XDG_RUNTIME_DIR` isn't set and of providing a replacement directory. Any application where that behaviour would be relevant can use the exception raised by `getRuntimeDir` to detect this problem, should deal with warning the user in its own context, and can set `$XDG_RUNTIME_DIR` with a path that makes sense for that application.

# Build

You can build the project with:

```
hpack
cabal build
```

If you have Nix installed, `shell.nix` provides an environment with all the needed dependencies. It is suitable for `nix-direnv`.
