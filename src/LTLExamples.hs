module LTLExamples where
import LTL

decreasing :: (Ord a) => LTL a
decreasing = Terminated .| Atomic (\x -> X (Terminated .| ((Atomic $ \y -> fromBool $ y < x) .& decreasing)))
