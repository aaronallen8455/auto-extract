# auto-extract :fork_and_knife:

This is a GHC plugin that edits the file being compiled by extracting a target
expression as a top level declaration. This is useful for breaking up large
functions into smaller reusable pieces.

### Usage

This plugin is intended to be used with GHCi or adjacent utilities such as
`ghcid` and `ghciwatch` as a developement tool, not as a package dependency.
Here is an example command for starting a REPL for a stack project with the
`auto-extract` plugin enabled (you may need to add `auto-extract` to your
`extra-deps` first):

```
stack repl my-project --package auto-extract --ghci-options='-fplugin AutoExtract'
```

likewise for a cabal project (you may need to run `cabal update` first):

```
cabal repl my-project --build-depends auto-extract --repl-options='-fplugin AutoExtract'
```

With the plugin enabled, mark the expression to be extracted using
`EXTRACT@name (<expr>)` then compile the module. A top level definition will be
added identified by the given name and having the expression enclosed in parens
as the body. The plugin will determine what arguments are necessary and will
include a type signature based on the inferred type of the expression.
Compilation is aborted after the code is updated.

For example, given this program:

```haskell
doubleInput :: IO ()
doubleInput = do
  input <- do
    putStrLn "Enter a number"
    readLn
  let doubled = input * 2
  print doubled
```

Insert the extraction targets:

```haskell
doubleInput :: IO ()
doubleInput = do
  input <- EXTRACT@promptNumber (do
    putStrLn "Enter a number"
    readLn)
  let doubled = EXTRACT@double (input * 2)
  print doubled
```

Note: the code can contain multiple `EXTRACT` directives and they can be nested
within one another.

Compiling the module will result in the source code being updated to:

```haskell
doubleInput :: IO ()
doubleInput = do
  input <- promptNumber
  let doubled = double input
  print doubled

promptNumber :: IO Integer
promptNumber = do
  putStrLn "Enter a number"
  readLn

double :: Num a => a -> a
double input = input * 2
```

### Caveats

- The resulting module must pass the type checker in order for the code to be
  updated.
- The plugin only supports certain GHC versions with the intent of supporting
  the four latest release series, i.e. `9.6.*` thru `9.12.*`. The cabal file
  indicates the currently supported versions.
