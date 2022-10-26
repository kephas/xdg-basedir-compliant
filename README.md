# XDG Basedir implementation

This is a compliant implementation of the [XDG Base Directory Specification](https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html).

## Compliance

- [ ] `$XDG_DATA_HOME`
  - [X] get path
  - [X] read file
  - [ ] write file
- [ ] `$XDG_CONFIG_HOME`
  - [X] get path
  - [X] read file
  - [ ] write file
- [ ] `$XDG_STATE_HOME`
  - [X] get path
  - [ ] read file
  - [ ] write file
- [X] `$XDG_DATA_DIRS`
  - [X] get path list
  - [X] read best file
  - [X] merge files
- [ ] `$XDG_CONFIG_DIRS`
  - [X] get path list
  - [X] read best file
  - [X] merge files
- [ ] `$XDG_CACHE_HOME`
  - [ ] get path
  - [ ] read file
  - [ ] write file
- [ ] `$XDG_RUNTIME_DIR`
  - [ ] get path
  - [ ] read file
  - [ ] write file

# Build

You can build the project with:

```
hpack
cabal build
```

If you have Nix installed, `shell.nix` provides an environment with all the needed dependencies. It is suitable for `nix-direnv`.
