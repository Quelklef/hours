module Hours.Display.Util where

import Prelude

import Data.Foldable (class Foldable, foldMap)
import Data.Ord.Max (Max(..))
import Data.Tuple.Nested (type (/\), (/\))
import Data.Newtype (un)
import Data.String.Common (split) as Str
import Data.String.Pattern (Pattern(..)) as Str
import Data.String.CodeUnits (length) as Str
import Data.Array (intercalate)
import Data.Monoid (power)

import Hours.Time (Minutes(..), Instant, asMilliseconds)

foldMax :: forall f k a. Foldable f => Bounded k => Ord k => (a -> k) -> f a -> k
foldMax f l = l # foldMap (Max <<< f) # un Max

indent :: String -> String -> String
indent dent = Str.split (Str.Pattern "\n") >>> map (dent <> _) >>> intercalate "\n"

justifyRight :: Int -> String -> String
justifyRight n s = " " `power` (n - Str.length s) <> s

justifyLeft :: Int -> String -> String
justifyLeft n s = s <> " " `power` (n - Str.length s)

surroundWith :: forall m. Semigroup m => m -> m -> m
surroundWith s c = s <> c <> s

displayPairs :: Array (String /\ String) -> String
displayPairs pairs =
  let leftWidth = pairs # foldMax (\(k /\ _) -> Str.length k) in
  pairs # map (\(k /\ v) -> justifyRight leftWidth k <> ": " <> v)
        # intercalate "\n"

--

foreign import displayMillisecondsAsDateTime :: Number -> String

displayInstant_DateTime :: Instant -> String
displayInstant_DateTime = asMilliseconds >>> displayMillisecondsAsDateTime

foreign import displayMillisecondsAsHHMM :: Number -> String

displayInstant_HHMM :: Instant -> String
displayInstant_HHMM = asMilliseconds >>> displayMillisecondsAsHHMM

foreign import displayMillisecondsAsDate :: Number -> String

displayInstant_Date :: Instant -> String
displayInstant_Date = asMilliseconds >>> displayMillisecondsAsDate

displayMinutes :: Minutes -> String
displayMinutes (Minutes n) =
  let hours = n `div` 60
      minutes = n `mod` 60
  in show hours <> "h " <> show minutes <> "m"
