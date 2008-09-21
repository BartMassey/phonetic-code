--- Spelling word suggestion tool
--- Copyright © 2008 Bart Massey
--- ALL RIGHTS RESERVED

--- This software is licensed under the "3-clause ('new')
--- BSD License".  Please see the file COPYING provided with
--- this distribution for license terms.

-- |The idea and name for thimk came from an old program that used to hang
--  around Reed College, probably written by Graham Ross and
--  now apparently lost in the mists of time.
--  See
--    http://groups.google.com/group/net.sources/msg/8856593862fe22bd
--  for the one very vague reference I've found on the web (in the
--  SEE ALSO section of the referenced manpage).
-- 
--  The current implementation is a bit more sophisticated
--  than I recall the original being. By
--  default it uses a prefilter that discards words with
--  large edit distances from the target, then filters words
--  with a different phonetic code than the target, then
--  presents the top result sorted by edit distance.
-- 
--  The Soundex and Phonix phonetic codes are designed for
--  names, but seem to work about the same with other words.
--  I follow the common practice of not truncating the codes
--  for greater precision, although Phonix does truncate its
--  final "sound" for greater recall.

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
              | ArgChoices
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
               argDesc = "Phonetic code: one of soundex, phonix"},
         Arg { argIndex = ArgChoices,
               argName = Nothing,
               argAbbr = Just 'n',
               argData = argDataDefaulted "choices" ArgtypeInt 1,
               argDesc = "Max number of choices to offer"},
         Arg { argIndex = ArgNoPrefilter,
               argName = Just "no-distance-prefilter",
               argAbbr = Nothing,
               argData = Nothing,
               argDesc = "Do not discard wildly misspelled words early"},
         Arg { argIndex = ArgWord,
               argName = Nothing,
               argAbbr = Nothing,
               argData = argDataRequired "word" ArgtypeString,
               argDesc = "Word to be looked up" } ]

edit_distance s t =
    restrictedDamerauLevenshteinDistance ec s t where
        ec = EditCosts {
               insertionCost = 2,
               deletionCost = 2,
               transpositionCost = 1,
               substitutionCost = 3 }

try_word :: Bool -> (String -> String) -> Int -> String -> String -> String
try_word prefilter pcode choices word = unlines
              . take choices
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
  let choices = getRequiredArg args ArgChoices
  let result = try_word prefilter pcode choices word dict
  putStr result
