{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards   #-}

module Game where

import BasePrelude
import Control.Monad.Random.Class

import Cave
import Common

-- | The true internal game state. Compare with 'View', which is the player's
-- view of the game state.
data Game = Game
    { gPlayer :: Cave    -- ^ player location
    , gWumpus :: Cave    -- ^ wumpus location
    , gMap    :: CaveMap -- ^ the cave map
    }
    deriving Show

-- | A test game state.
triGame :: Game
triGame = Game
    { gPlayer = cave0
    , gWumpus = cave1
    , gMap    = triCave
    }

-- | Turn a game state into a player's view of that game state.
--
-- >>> mkView triGame
-- View {vPlayer = 0, vTunnels = [1,2], vWumpus = True}
mkView :: Game -> View
mkView g@Game {..} = undefined

-- | Given a 'CaveMap', create a 'Game'. This is wrapped in a 'MonadRandom'
-- context, because we want to start the player and the wumpus each at a
-- different random cave. How can we implement this?
--
--   * Use 'rndCave' twice, to get a player cave and a wumpus cave.
--   * If those caves are the same, try again.
--   * If they are different, return the new 'Game'.
--
-- If you are having difficulty, just start by having your function return a
-- new 'Game' in the monadic context, using 'cave0' and 'cave1' for the
-- locations:
--
-- > mkGame c = ... (Game { gPlayer = cave0, gWumpus = cave1, gMap = c }) ...
--
-- That will help you get the types right. Then add the use of 'rndCave'.
mkGame :: MonadRandom m => CaveMap -> m Game
mkGame c = undefined

-- | Apply an action to the game state.
--
-- You can only move to an adjacent cave. If you move to the Wumpus, you are
-- eaten. Otherwise, return a new 'Game' with the new player location.
--
-- You can only shoot an adjacent cave. If you shoot the Wumpus, you win! If
-- you miss, you lose.
--
-- Note that move and shoot will both want to check their target location to
-- see if it is adjacent. See if you can use the same code for both checks, or
-- combine both checks into one.
--
-- >>> doAction triGame Quit
-- Just (Right Nothing)
-- >>> doAction triGame (Move cave0)
-- Nothing
-- >>> doAction triGame (Move cave1)
-- Just (Left Eaten)
-- >>> fmap (fmap gPlayer) <$> doAction triGame (Move cave2)
-- Just (Right (Just 2))
-- >>> doAction triGame (Shoot cave0)
-- Nothing
-- >>> doAction triGame (Shoot cave1)
-- Just (Left Hit)
-- >>> doAction triGame (Shoot cave2)
-- Just (Left Miss)
doAction :: Game -> Action -> Status Game
doAction g@Game {..} = undefined
