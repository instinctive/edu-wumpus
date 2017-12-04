{-# LANGUAGE NoImplicitPrelude #-}

module Play where

import BasePrelude

import Cave
import Game
import Input
import Output

-- | Play a game of Hunt the Wumpus using the 'CaveMap'.
--
-- The basic flow of this function:
--
--   * Create the 'Game' from the 'CaveMap'.
--   * Use 'printView' to show the player's view.
--   * Get the player action with 'getAction', and apply it to the game state.
--     * If the player action is invalid, use 'printInvalid' and try again.
--     * If the game state is a (game-ending) message, print it (and return
--     from 'play' with @()@, ending the game.
--     * If the player quit @(Right Nothing)@, return @()@ (ending the game).
--     * Otherwise, repeat with the new game state.
play :: CaveMap -> IO ()
play c = undefined
