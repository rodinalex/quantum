{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module TypeClasses where

-- class QuantumUnit a where
--   type ConjType a
--   hC :: a -> ConjType a
--   (|+|) :: a -> a -> a
--   (|-|) :: a -> a -> a
--
-- class Scalable a b where
--   type ProdTy a b
--   (|*|) :: a -> b -> ProdTy a b
--
-- class BraKet a where
--   normalize :: a -> a
--
-- class (QuantumUnit a) => OperatorClass a where
--     type StateType a
--     qO :: a -> (StateType a -> Map (StateType a) Amplitude)
--     -- infixr 6 |->|
--     -- (|->|) :: (Eq (StateType a), Ord (StateType a)) => a -> Ket (StateType a) -> Ket (StateType a)
--     -- (|->|) oP (Ket kt) =
--     --     Protolude.foldl (|+|) (Ket Map.empty) $ (\(state, amp) -> (|*|) amp $ Ket $ qO oP state ) <$> Map.toList kt
--     -- infixl 6 |<-|
--     -- (|<-|) :: (Eq (StateType a), Ord (StateType a)) =>  Bra (StateType a) -> a -> Bra (StateType a)
--     -- (|<-|) b oP =
--     --   toBra (hC oP |->| toKet b)
