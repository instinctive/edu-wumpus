{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards   #-}

module Cave
    -- Here's how to hide stuff from external modules:
    -- ( Cave(Cave), cave0, cave1, cave2
    -- , CaveMap, tunnels, rndCave
    -- , triCave, cubeCave
    -- )
    where

import BasePrelude
import Control.Monad.Random.Class
import Data.Map (Map, (!))
import qualified Data.Map as M

-- * The 'Cave' type

-- | Caves are represented as integers, but by using a @newtype@:
--
--   * A 'Cave' can never be confused with an 'Int'.
--   * We hide the 'getCave' accessor outside of this module.
newtype Cave = Cave { getCave :: Int }
    deriving (Eq, Ord)

-- | Create a 'Show' instance for caves that just shows the cave number.
--
-- >>> show (Cave 2)
-- "2"
instance Show Cave where
    show = undefined

-- | Some caves to be used in tests.
--
-- >>> unwords $ show <$> [cave0,cave1,cave2]
-- "0 1 2"
cave0, cave1, cave2 :: Cave
cave0 = undefined
cave1 = undefined
cave2 = undefined

-- * The 'CaveMap' type

-- | The use of @newtype@ allows us to prevent access to the internal 'Map'
-- outside of this module.
newtype CaveMap = CaveMap { getCaveMap :: Map Cave [Cave] }
    deriving Show

-- | Return the caves adjacent to the given cave.
--
-- To implement this, you will need to use the internal 'Map' inside the
-- 'CaveMap'.
--
-- >>> tunnels triCave cave0 == [cave1,cave2]
-- True
tunnels :: CaveMap -> Cave -> [Cave]
tunnels m = undefined

-- | Return a random cave. The result is wrapped in a 'MonadRandom' context.
--
-- How will you implement this? You need two pieces:
--
--   * A list of all the caves in the 'CaveMap'. Use 'getCaveMap' and a
--   function from 'Data.Map'.
--   * A random number in a certain range. Use 'getRandomR' from
--   'Control.Monad.Random.Class'.
rndCave :: MonadRandom m => CaveMap -> m Cave
rndCave CaveMap {..} = undefined

-- | A simple test map that connects 'cave0', 'cave1', and 'cave2'.
--
-- >>> tunnels triCave cave0
-- [1,2]
-- >>> tunnels triCave cave1
-- [2,0]
-- >>> tunnels triCave cave2
-- [0,1]
triCave :: CaveMap
triCave = undefined

-- | A map with eight caves, arranged as the vertices of a cube.
--
-- See if you can figure out a way to create it without just building the map
-- explicitly.  Hint: a cube is like two rings of four vertices each, where
-- each element of a ring is linked to the element next to it, and to its
-- partner in the other ring. Also note that every link is bi-directional.
--
-- >>> M.keys (getCaveMap cubeCave)
-- [0,1,2,3,4,5,6,7]
-- >>> length <$> M.elems (getCaveMap cubeCave)
-- [3,3,3,3,3,3,3,3]
-- >>> concat $ fmap show $ sort $ concat $ M.elems (getCaveMap cubeCave)
-- "000111222333444555666777"
cubeCave :: CaveMap
cubeCave = undefined
