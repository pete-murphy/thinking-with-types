## Exercises

#### 14.-i

> Use Curry-Howard to prove that `(a^b)^c == a^(b*c)` 

```haskell
to   :: (b -> c -> a) -> ((b, c) -> a)
to bca (b,c) = bca b c
from :: ((b, c) -> a) -> (b -> c -> a)
from bca b c = bca (b,c)
```

#### 14.-ii

> Give a proof of the exponent law that `a^b * a^c = a^(b+c)`

This would mean there's an isomorphism between
```
(b -> a, c -> a)
```
and
```
Either b c -> a
```

```haskell
to :: (b -> a, c -> a) -> (Either b c -> a)
to (ba,ca) = \case
  Left b -> ba b
  Right a -> ca c
to (ba,ca) = either ba ca

from :: (Either b c -> a) -> (b -> a, c -> a)
from bca = (\b -> bca (Left b), \c -> bca (Right c))
from bca = (bca . Left, bca . Right)
```

#### 14.-iii

> Prove `(a*b)^c = a^c * b^c`

This would mean there's an isomorphism between
```
c -> (a, b)
```
and
```
(c -> a, c -> b)
```

```haskell
to :: (c -> (a, b)) -> (c -> a, c -> b)
to cab = (\c -> fst (cab c), \c -> snd (cab c))
to cab = (fst . cab, snd . cab)

from :: (c -> a, c -> b) -> (c -> (a, b))
from (ca,cb) c = (ca c, cb c)
```
