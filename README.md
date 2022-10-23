# XDG Basedir implementation

This is a compliant implementation of the [XDG Base Directory Specification](https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html).

# Build

You can build the project with:

```
hpack
cabal build
```

If you have Nix installed, `shell.nix` provides an environment with all the needed dependencies. It is suitable for `nix-direnv`.
