Zifter
======


- Write your own pre-commit tests with a simple EDSL
- Automatic parallelisation
- Linear-looking output
- Only one dependency: [`stack`](https://haskellstack.org/)


### What to do with a `zift.hs` script when it's in a repository

A `zift.hs` script is a code quality tool.
It can be used to define and enforce code quality standards in a repository.

To run a `zift.hs` script, execute `./zift.hs run`.
This will run the preprocessor, the prechecker and the checker.
If any point something fails, the entire execution will fail and the exit code will be nonzero.

To start using a `zift.hs` script automatically, you need to install as a pre-commit script with `zift.hs install`.
This ensures that `zift.hs run` will be run before every `git commit`, and that you will not be able to commit unless `zift.hs run` exits successfully.
(There is still a way around this with `git commit --no-verify` for emergencies.)

You can also run individual parts of the `zift.hs` script with `zift.hs preprocess`, `zift.hs precheck" and `zift.hs check".

### Example

[`zift.hs`](/zift.hs)

The following zift script first runs `hindent` and `cabal format` on all relevant files, in parallel.
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

- Install the zift script as a pre-commit hook with `./zift.hs install`.
- Run the zift script manually with `./zift.hs run`.

### The name

The name 'Zifter' comes from the dutch word 'muggenzifter', which means 'nitpicker'.
