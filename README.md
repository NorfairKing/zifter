Zifter
======


- Write your own pre-commit tests for any project with a simple EDSL
- Automatic parallelisation
- Output that looks as if everything was run sequentially.
- Only one dependency: [`stack`](https://haskellstack.org/)


### What to do with a `zift.hs` script when it's in a repository

A `zift.hs` script is a code quality tool.
It can be used to define and enforce code quality standards in a repository.

To run a `zift.hs` script, execute `./zift.hs run`.
This will run the preprocessor, the prechecker and the checker.
If atany point something fails, the entire execution will fail and the exit code will be nonzero.

To start using a `zift.hs` script automatically, you need to install it as a pre-commit script with `zift.hs install`.
This ensures that `zift.hs run` will be run before every `git commit`, and that you will not be able to commit unless `zift.hs run` exits successfully.
(There is still a way around this with `git commit --no-verify` for emergencies.)

You can also run individual parts of the `zift.hs` script with `zift.hs preprocess`, `zift.hs precheck` and `zift.hs check`.

### How to write a `zift.hs` script

Zifter is intended to be a composable (and extensible) code quality tool.
You can write your own `zift.hs` scripts to define the code quality standards that you want to uphold in your project.

A `zift.hs` has three main sections:

- `preprocessor`
- `prechecker`
- `checker`

In the `preprocess` section, you define what needs to happen with your code before you start checking anything.
For example, this is where you put code formatters.

In the `prechecker` section, you define what needs to happen with the repository after the preprocessor check.
For example, after you have reformatted all the source files, you will probably want to run `git add .`, so that these changes are also in the commit.

In the `checker` section, you define all the checks that need to succeed in order to allow a commit.
For example, you probably want to ensure that the project builds successfully, that the tests pass, etc...

To find predefined functions that you can put in these sections, have a look at the `zifter-*` packages.

#### Example

The following is an example of a `zift.hs` script for a Haskell project.

[`zift.hs`](/zift.hs)

The following `zift.hs` script first runs `hindent` and `cabal format` on all relevant files, in parallel.
Then it runs `git add .`.
Lastly, it runs `hlint` on the entire project and then it runs `stack` to ensure that everything compiles without warnings and the tests succeed.

```
#!/usr/bin/env stack
{- stack
    --install-ghc
    runghc
    --package zifter
    --package zifter-cabal
    --package zifter-git
    --package zifter-hindent
    --package zifter-hlint
    --package zifter-stack
-}

import Zifter
import Zifter.Cabal
import Zifter.Git
import Zifter.Hindent
import Zifter.Hlint
import Zifter.Stack

main :: IO ()
main =
    ziftWith $ do
        preprocess $ ziftP [hindentZift, cabalFormatZift]
        precheck gitAddAllZift
        checker $ do
            hlintZift
            stackBuildZift
```

### How to write your own `Zift` functions

The functions in the `preprocess`, `precheck` and `checker` sections are of type `Zift ()`.

All the relevant documentation is in [the `Zifter` module](https://hackage.haskell.org/package/zifter/docs/Zifter.html).

It is important to note that `Zift` has a `MonadIO` instance, so that you can embed any `IO` action in a `Zift` action.

### How to use a `zift.hs` script to define your continuous integration setup

The most important parts here are the following:

- Make sure you get `stack`:

```
addons:
  apt:
    packages:
    - libgmp-dev

before_install:
  - mkdir -p ~/.local/bin
  - export PATH=$HOME/.local/bin:$PATH
  - travis_retry curl -L https://www.stackage.org/stack/linux-x86_64 | tar xz --wildcards --strip-components=1 -C ~/.local/bin '*/stack'
  - chmod a+x ~/.local/bin/stack
```

Install all dependencies with stack:

```
install:
  - stack  setup
  - stack  build --only-snapshot
```

Run the `zift.hs` script on travis.

```
script:
  - ./zift.hs run
```

The [`.travis.yml`](/.travis.yml) file in this repository can serve as an example.

### The name

The name 'Zifter' comes from the dutch word 'muggenzifter', which means 'nitpicker'.
