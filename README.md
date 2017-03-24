Zifter
======


- Write your own pre-commit tests with a simple EDSL
- Automatic parallelisation
- Linear-looking output
- Only one dependency: [`stack`](https://haskellstack.org/)


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
        preprocessor $ ziftP [hindentZift, cabalFormatZift]
        precheck gitAddAllZift
        checker $ do
            hlintZift
            stackBuildZift
```

- Install the zift script as a pre-commit hook with `./zift.hs install`.
- Run the zift script manually with `./zift.hs run`.

### The name

The name 'Zifter' comes from the dutch word 'muggenzifter', which means 'nitpicker'.
