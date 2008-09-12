--- Spelling word suggestion tool
--- Copyright © 2008 Bart Massey
--- ALL RIGHTS RESERVED

--- This software is licensed under the "3-clause ('new')
--- BSD License".  Please see the file COPYING provided with
--- this distribution for license terms.

--- Idea and name from an old program that used to hang
--- around Reed College, probably written by Graham Ross and
--- now apparently lost in the mists of time.
--- See
---   http://groups.google.com/group/net.sources/msg/8856593862fe22bd
--- for the one very vague reference I've found on the web (in the
--- SEE ALSO section of the referenced manpage).

--- Soundex is designed for names, but seems to work about the same
--- with other words.  I allow it to do arbitrary-length
--- codes instead of length 4 by default, to get better accuracy.

import System.Console.ParseArgs
import Data.Soundex

data ArgIndex = ArgWord deriving (Eq, Ord, Show)

argd = [ Arg { argIndex =ArgWord,
               argName = Nothing,
               argAbbr = Nothing,
               argData = argDataRequired "word" ArgtypeString,
               argDesc = "Word to be looked up." } ]

main = do
  args <- parseArgsIO ArgsComplete argd
  print $ soundex True (getRequiredArg args ArgWord)
