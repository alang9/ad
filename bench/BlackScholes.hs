{-# LANGUAGE RankNTypes #-}
import Criterion.Main
import Data.Number.Erf
import qualified Numeric.AD as AD
import qualified Numeric.AD.Mode.Forward as Forward
import qualified Numeric.AD.Mode.Kahn as Kahn
import qualified Numeric.AD.Mode.Reverse as Reverse
import qualified Numeric.AD.Mode.Sparse as Sparse
import Numeric.AD.Types (AD, Mode)

blackScholes :: (Erf a) => a -> a -> a -> a -> a -> (a, a)
blackScholes r s v t k = (put, call)
  where
    put = k * exp (negate r * t) - s + call
    call = normcdf (negate d2) * k * exp (negate r * t) - normcdf (negate d1) * s
    d1 = (log (s / k) + (r + v * v / 2) * t) / (v * sqrt t)
    d2 = d1 - v * t

bs :: Erf a => [a] -> (a, a)
bs [r', s', v', t', k'] = blackScholes r' s' v' t' k'

fromPair (a, b) = [a, b]

greeks :: Erf a => ((forall m. Mode m => [AD m a] -> [AD m a]) -> [a] -> [[a]])
       -> a -> a -> a -> a -> a -> [[a]]
greeks jacobian r s v t k = jacobian (fromPair . bs) [r, s, v, t, k]

higherGreeks :: (Erf a) => ((forall m. Mode m => [AD m a] -> AD m a) -> [a] -> [[a]])
             -> a -> a -> a -> a -> a -> ([[a]], [[a]])
higherGreeks hessian r s v t k =
    ( hessian (fst . bs) [r, s, v, t, k]
    , hessian (snd . bs) [r, s, v, t, k] )

highererGreeks :: (Erf a) => ((forall m. Mode m => [AD m a] -> [AD m a]) -> [a] -> [[[a]]])
               -> a -> a -> a -> a -> a -> [[[a]]]
highererGreeks hessianF r s v t k = hessianF (fromPair . bs) [r, s, v, t, k]

runF :: Num a => (a -> a -> a -> a -> a -> b) -> Int -> [b]
runF f n =
    [ f r s v t k
    | r <- xs, s <- xs, v <- xs, t <- xs, k <- xs]
  where
    xs = map fromIntegral [1..n]

runFloat :: (Float -> Float -> Float -> Float -> Float -> b) -> Int -> [b]
runFloat = runF

runDouble :: (Double -> Double -> Double -> Double -> Double -> b) -> Int -> [b]
runDouble = runF

main = defaultMain
    [ bgroup "Forward"
        [ bench "greeks Double" $ nf (runDouble (greeks Forward.jacobian)) 2
        , bench "greeks Float" $ nf (runFloat (greeks Forward.jacobian)) 2
        ]
    , bgroup "Kahn"
        [ bench "greeks Double" $ nf (runDouble (greeks Kahn.jacobian)) 2
        , bench "higherGreeks Double" $ nf (runDouble (higherGreeks Kahn.hessian)) 2
        , bench "highererGreeks Double" $ nf (runDouble (highererGreeks Kahn.hessianF)) 2
        , bench "greeks Float" $ nf (runFloat (greeks Kahn.jacobian)) 2
        , bench "higherGreeks Float" $ nf (runFloat (higherGreeks Kahn.hessian)) 2
        , bench "highererGreeks Float" $ nf (runFloat (highererGreeks Kahn.hessianF)) 2
        ]
    , bgroup "Reverse"
        [ bench "greeks Double" $ nf (runDouble (greeks Reverse.jacobian)) 2
        , bench "higherGreeks Double" $ nf (runDouble (higherGreeks Reverse.hessian)) 2
        , bench "highererGreeks Double" $ nf (runDouble (highererGreeks Reverse.hessianF)) 2
        , bench "greeks Float" $ nf (runFloat (greeks Reverse.jacobian)) 2
        , bench "higherGreeks Float" $ nf (runFloat (higherGreeks Reverse.hessian)) 2
        , bench "highererGreeks Float" $ nf (runFloat (highererGreeks Reverse.hessianF)) 2
        ]
    , bgroup "Sparse"
        [ bench "greeks Double" $ nf (runDouble (greeks Sparse.jacobian)) 2
        , bench "higherGreeks Double" $ nf (runDouble (higherGreeks Sparse.hessian)) 2
        , bench "highererGreeks Double" $ nf (runDouble (highererGreeks Sparse.hessianF)) 2
        , bench "greeks Float" $ nf (runFloat (greeks Sparse.jacobian)) 2
        , bench "higherGreeks Float" $ nf (runFloat (higherGreeks Sparse.hessian)) 2
        , bench "highererGreeks Float" $ nf (runFloat (highererGreeks Sparse.hessianF)) 2
        ]
    , bgroup "Mixed"
        [ bench "greeks Double" $ nf (runFloat (greeks AD.jacobian)) 2
        , bench "higherGreeks Double" $ nf (runFloat (higherGreeks AD.hessian)) 2
        , bench "highererGreeks Double" $ nf (runFloat (highererGreeks AD.hessianF)) 2
        , bench "greeks Float" $ nf (runFloat (greeks AD.jacobian)) 2
        , bench "higherGreeks Float" $ nf (runFloat (higherGreeks AD.hessian)) 2
        , bench "highererGreeks Float" $ nf (runFloat (highererGreeks AD.hessianF)) 2
        ]
    ]
