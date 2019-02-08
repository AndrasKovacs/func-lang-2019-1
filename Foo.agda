

open import Data.Product

data Cmp : Set where LT EQ GT : Cmp




data Map (K : Set)(cmp : K → K → Cmp)(V : Set) : Set where
  nil  : Map K cmp V
  node : Map K cmp V → Map K cmp V → K → V → Map K cmp V

insert : ∀ {K cmp V} → K → V → Map K cmp V → Map K cmp V
insert k v nil              = node {!!} {!!} {!!} {!!}
insert k v (node l r k' v') = {!!}

Map' : Set → Set → Set
Map' K V = Σ (K → K → Cmp) (λ cmp → Map K cmp V)

-- merge : ∀ {K V} → (Map' K V → Map' K V → Map' (K × K) V
-- merge = {!!}

-- data Map' :: * -> * -> * where
--   mkMap :: forall K V. (cmp : K -> K -> Cmp) → Map K cmp V → Map' K V
