--- Soundex code calculator
--- Copyright © 2008 Bart Massey
--- ALL RIGHTS RESERVED

--- Based on description of soundex at
---   http://wikipedia.org/Soundex
--- and in Knuth's "The Art of Computer Programming" 2nd ed
--- v1 pp394-395

--- Originally written for the "thimk" spelling suggestion
--- application in Nickle (http://nickle.org) in July 2002
--- based on a description at
---   http://www.searchforancestors.com/soundex.html
--- and ported September 2008; the other variants were also
--- added at this time.

--- This software is licensed under the "3-clause ('new')
--- BSD License".  Please see the file COPYING provided with
--- this distribution for license terms.

module Data.Soundex (soundexCodes, soundex, soundexCensus)
where

import Data.List
import Data.Char
import Data.Array.IArray
import Data.Maybe

-- | Soundex codes for single characters.
soundexCodes :: Array Char Char
soundexCodes = accumArray updater '?' ('A', 'Z') codes where
    updater '?' c = c
    updater _ c = error ("updater called twice on " ++ [c])
    groups = [('1', "BFPV"),
              ('2', "CGJKQSXZ"),
              ('3', "DT"),
              ('4', "L")
              ('5', "MN"),
              ('6', "R",
              ('7', "HW")]   --- group 7 is just a coding convenience
    codes = concatMap make_codes groups
    make_codes (s, i) = zip s (repeat i)

-- | Compute a "full" soundex code; i.e., do not drop any
-- encodable characters from the result.  The leading
-- character of the code will be folded to uppercase.
-- Non-alphabetics are not encoded. If no alphabetics are
-- present, the soundex code will be "0".
soundex :: String -> String
soundex = filter (/= '?')
        . filter_multiples
        . filter (/= '7')
        . encode
        . map toUpper
        . dropWhile (not . isAlpha)
    where
      filter_multiples = map head . group
      encode "" = "0"
      encode (a : as) = a : map unsound as
      unsound c | isAlpha c = soundexCodes ! c
      unsound _ = '?'

--- | This is the most common US census variant of `soundex`,
--- compatible with most existing calculators.  It gives the
--- first four characters of the full soundex code, zero-padded
--- as needed.
soundexCensus :: String -> String
soundexCensus = take 4 . (++ repeat '0') . soundex
