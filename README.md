# haskell-starter

In `~/`:

```
stack --resolver lts-9.21 setup --reinstall
stack install --resolver lts-9.21 ghc-mod
stack install hlint
```

In the project:

```
stack build --file-watch --fast --exec haskell-starter-exe
```
