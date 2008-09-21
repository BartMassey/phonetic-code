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
import Data.List
import Data.Ord

import Text.EditDistance
import System.Console.ParseArgs

import Text.PhoneticCode.Soundex
import Text.PhoneticCode.Phonix

data ArgIndex = ArgWord
              | ArgCode
              | ArgNoPrefilter
              | ArgDict
                deriving (Eq, Ord, Show)

argd = [ Arg { argIndex = ArgDict,
               argName = Just "dictionary",
               argAbbr = Just 'd',
               argData = argDataDefaulted "path" ArgtypeString
                           "/usr/share/dict/words",
               argDesc = "Dictionary file to search" },
         Arg { argIndex = ArgCode,
               argName = Just "code",
               argAbbr = Just 'c',
               argData = argDataDefaulted "phonetic-code" ArgtypeString
                           "phonix",
               argDesc = "Phonetic code: soundex, phonix"},
         Arg { argIndex = ArgNoPrefilter,
               argName = Just "no-distance-prefilter",
               argAbbr = Nothing,
               argData = Nothing,
               argDesc = "Do not throw away wildly misspelled words before looking further."},
         Arg { argIndex = ArgWord,
               argName = Nothing,
               argAbbr = Nothing,
               argData = argDataRequired "word" ArgtypeString,
               argDesc = "Word to be looked up." } ]

edit_distance s t =
    restrictedDamerauLevenshteinDistance ec s t where
        ec = EditCosts {
               insertionCost = 2,
               deletionCost = 2,
               transpositionCost = 1,
               substitutionCost = 3 }

try_word :: Bool -> (String -> String) -> String -> String -> String
try_word prefilter pcode word = unlines
              . take 5
              . sortBy (comparing ed)
              . map snd
              . filter ((== pcode word) . fst)
              . map sfs
              . prefilter_f
              . lines
    where
      sfs w = (pcode w, w)
      ed = edit_distance word
      prefilter_f = if prefilter then filter ((<= 10) . ed) else id

main = do
  args <- parseArgsIO ArgsComplete argd
  h <- argFileOpener (getRequiredArg args ArgDict) ReadMode 
  dict <- hGetContents h
  let pcode = case getRequiredArg args ArgCode of
                "soundex" -> soundex True
                "phonix" -> phonix
                c -> usageError args $ "unknown phonetic code " ++ c
  let prefilter = not (gotArg args ArgNoPrefilter)
  let word = getRequiredArg args ArgWord
  let result = try_word prefilter pcode word dict
  putStr result
