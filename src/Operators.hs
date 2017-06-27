{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
-- {-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE TypeFamilies        #-}
-- {-# LANGUAGE TypeOperators       #-}

module Operators where

import           BraKet
import           Data.List
import           Protolude

-- data Direction = X
--                | Y
--                | Z
--
-- data Spin a = SpinUp a
--             | SpinDown a


-- data QOperator b c = QOperator {qOperator :: [b -> [QState c]]}

-- class QOperator a b c where
--   hC :: a b c -> a b c
--   qO :: a b c -> (b -> [QState c])
--   (|->|) :: a b c -> Ket b -> Ket c
--   (|->|) oP Ket {ket = qStates} =
--     Ket $ concatMap (helper $ qO oP) qStates
--       where
--         helper z (x:|y)= fmap (scaleState x) (z y)
--   (|<-|) :: Bra b -> a b c -> Bra c
--   (|<-|) b oP = toBra (hC oP |->| toKet b)
--
-- data SpinOperator b c = SpinOperator (b -> [QState c])
--
-- instance QOperator SpinOperator b c where
--   hC a = a
--   qO (SpinOperator f) = f

-- instance QOperator SpinOperator where
--   hC (SpinOperator f) = SpinOperator f
--   qO (SpinOperator f) = f


-- data QO a b = QO (a -> [QState b])
--
-- spinFlip = QO (\s ->case s of
--                 SpinUp   -> [ (1 :+ 0) :|SpinDown]
--                 SpinDown -> [ (1 :+ 0) :|SpinUp])

-- type SpinOperator = QO (Spin -> [QState Spin])

-- data SpinFlip = SF (\s -> s)
-- data SpinFlip = SF { qO = (\s->[(1:+0):|s])}
          -- case s of
          -- SpinUp   -> [ (1 :+ 0) :|SpinDown]
          -- SpinDown -> [ (1 :+ 0) :|SpinUp])

-- data Spin = SpinUp
--           | SpinDown deriving(Show, Eq)


-- data SpinFlip a b = SpinFlip a b
--
-- instance QOperator (SpinFlip Spin Spin) where
--   hC (SpinFlip Spin Spin) = SpinFlip Spin Spin

--
-- instance QOperator SpinFlip where
--   hC SpinFlip = SpinFlip
--   qO SpinFlip s = case s of
--           SpinUp   -> [ (1 :+ 0) :|SpinDown]
--           SpinDown -> [ (1 :+ 0) :|SpinUp]

-- data QO b c = QO (b -> [QState c])
--
-- (|->|) :: (QOperator (QO b c)) => QO b c -> Ket b -> Ket c
--
--
-- (|<-|) :: (QOperator (QO b c)) => Bra b -> QO b c -> Bra c
-- (|<-|) b oP = toBra (hC oP |->| toKet b)


-- data SpinFlip = SpinFlip
--
-- instance QOperator SpinFlip where
--   hC SpinFlip = SpinFlip
--   qO SpinFlip s =
--     case s of
--         SpinUp   -> [ (1 :+ 0) :|SpinDown]
--         SpinDown -> [ (1 :+ 0) :|SpinUp]
-- --
-- -- spinFlip :: Spin -> [QState Spin]
-- -- spinFlip s =
-- --   case s of
-- --     SpinUp   -> [ (1 :+ 0) :|SpinDown]
-- --     SpinDown -> [ (1 :+ 0) :|SpinUp]
