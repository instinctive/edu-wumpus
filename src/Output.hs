{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards   #-}

module Output where

import BasePrelude
import System.IO (hFlush, stdout)

import Common

-- | Print the player's view of the game state.
--
-- >>> printView (Game.mkView Game.triGame)
-- You are in cave 0, tunnels lead to 1 2
-- You smell a wumpus!
printView :: View -> IO ()
printView View {..} = undefined

-- | Prompt the user for an action.
--
-- Your prompt should use 'putStr' rather than 'putStrLn', so that the user's
-- input is on the same line as the prompt.  But because the standard output is
-- line-buffered, you will have to explicitly tell it to "flush" the output
-- after you have printed the prompt.  You can do that with the following
-- incantation:
--
-- > import System.IO (hFlush, stdout) -- must be at the top of the file
-- ...
-- > printPrompt = ... hFlush stdout ...
--
-- >>> printPrompt
-- What do you do? 
-- >>> printPrompt >> printPrompt
-- What do you do? What do you do?
printPrompt :: IO ()
printPrompt = undefined

-- | Print a help message.
--
-- >>> printHelp
-- Valid commands:
--   quit
--   move <cave>
--   shoot <cave>
printHelp :: IO ()
printHelp = undefined

-- | Tell the user that their action was impossible.
--
-- >>> printInvalid
-- You can't do that.
printInvalid :: IO ()
printInvalid = undefined

-- | Print a game status message.
--
-- >>> traverse_ printMsg [Over,Miss,Hit,Eaten]
-- Game over, man, game over...
-- You missed! Unarmed and defenseless, you are Wumpus-fodder.
-- You killed the Wumpus! The villagers laud you as a hero!
-- You find the Wumpus! You are eaten as a tasty snack!
printMsg :: Msg -> IO ()
printMsg = undefined
