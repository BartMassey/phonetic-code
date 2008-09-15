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

import System.IO
import System.Console.ParseArgs
import Data.PhoneticCode.Soundex

data ArgIndex = ArgWord
              | ArgDict
                deriving (Eq, Ord, Show)

argd = [ Arg { argIndex = ArgDict,
               argName = Just "dictionary",
               argAbbr = Just 'd',
               argData = argDataDefaulted "path" ArgtypeString
                           "/usr/share/dict/words",
               argDesc = "Dictionary file to search" },
         Arg { argIndex = ArgWord,
               argName = Nothing,
               argAbbr = Nothing,
               argData = argDataRequired "word" ArgtypeString,
               argDesc = "Word to be looked up." } ]

try_word word = unlines
              . map snd
              . filter ((== sf word) . fst)
              . map sfs
              . lines
    where
      sf = soundex True
      sfs w = (sf w, w)

main = do
  args <- parseArgsIO ArgsComplete argd
  h <- argFileOpener (getRequiredArg args ArgDict) ReadMode 
  dict <- hGetContents h
  let word = getRequiredArg args ArgWord
  putStr $ try_word word dict
