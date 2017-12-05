{-# LANGUAGE NoImplicitPrelude #-}

module Parse where

import BasePrelude

import Cave
import Common

-- | Parse a 'String' into and 'Action', returning 'Nothing' if the string does
-- not represent a valid action.  To implement this, you will probably want to
-- use functions like 'words' and 'readMaybe'.
--
-- Note that @readMaybe :: Read a => String -> Maybe a@. You will use this to
-- parse the cave location for 'Move' and 'Shoot' actions. Since a successful
-- 'readMaybe' will return a @Just Int@, perhaps you can use 'fmap' or '<$>' to
-- avoid unwrapping and re-wrapping the 'Just'.
--
-- Since you will be taking user input, which is notorious for being
-- poorly-formed, you may want to canonicalize the input by mapping 'toLower'
-- over the characters.
--
-- In general, if there's anything suspicious about the input, such as extra
-- words, you should reject it. On the other hand, you might wish to allow any
-- prefix of the command to be accepted, Thus, "m 4" for "move 4", etc. If you
-- choose to implement this, you may find 'isPrefixOf' useful.
--
-- >>> parseAction "dsfaslk"
-- Nothing
-- >>> parseAction "quit"
-- Just Quit
-- >>> parseAction "quit sdf"
-- Nothing
-- >>> parseAction "move 4"
-- Just (Move 4)
-- >>> parseAction "move 4 3 2"
-- Nothing
-- >>> parseAction "shoot 4"
-- Just (Shoot 4)
-- >>> parseAction "shoot"
-- Nothing
parseAction :: String -> Maybe Action
parseAction = undefined

