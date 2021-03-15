#### 2.1.3-i
If `Show Int` has kind `CONSTRAINT`,
`Show` has kind `TYPE -> CONSTRAINT`

Whats the kind of Functor?
Functor :: (Type -> Type) -> Constraint

Whats the kind of Monad?
Monad :: (Type -> Type) -> Constraint

Whats the kind of MonadTrans?
MonadTrans :: ((Type -> Type) -> Type -> Type) -> Constraint
-- guessing?

