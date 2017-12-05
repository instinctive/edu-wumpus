{-# LANGUAGE NoImplicitPrelude #-}

module Common where

import BasePrelude

import Cave

-- | The game status is what gets communicated to the player from the game.
--
-- The outermost `Maybe` captures whether the player submitted a legal move. If
-- the move was invalid, say, trying to move to a non-adjacent cave, then the
-- whole status is `Nothing`.
--
-- The `Either` captures whether the game state is a message, which always
-- indicates a game-over condition in this simplified version of the game, or
-- an actual game state.
type Status a = Maybe (Either Msg a)

-- | The player's view of the game state.
data View = View
    { vPlayer  :: Cave   -- ^ player location
    , vTunnels :: [Cave] -- ^ adjacent caves
    , vWumpus  :: Bool   -- ^ is a wumpus nearby?
    }
    deriving Show

-- | Actions that the player can take.
data Action
    = Quit       -- ^ quit the game
    | Move Cave  -- ^ move to an adjacent cave
    | Shoot Cave -- ^ shoot into an adjacent cave
    deriving (Eq, Show)

-- | Messages about changes to the game state.
data Msg
    = Over  -- ^ game over (player quit)
    | Miss  -- ^ player missed and is unarmed
    | Hit   -- ^ player hit the wumpus and wins
    | Eaten -- ^ player is eaten by the wumpus
    deriving (Eq, Show)
