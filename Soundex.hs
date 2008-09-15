--- Soundex code calculator
--- Copyright © 2008 Bart Massey
--- ALL RIGHTS RESERVED

--- Based on description of soundex at
---   http://wikipedia.org/wiki/Soundex
--- and in Knuth's "The Art of Computer Programming" 2nd ed
--- v1 pp394-395.  A very helpful reference on the details
--- and differences among soundex algorithms is "Soundex:
--- The True Story",
---   http://west-penwith.org.uk/misc/soundex.htm
--- accessed 11 September 2008.

--- Originally written for the "thimk" spelling suggestion
--- application in Nickle (http://nickle.org) in July 2002
--- based on a description from
---   http://www.geocities.com/Heartland/Hills/3916/soundex.html
--- which is now
---   http://www.searchforancestors.com/soundex.html
--- Ported September 2008; the other variants were also
--- added at this time.

--- This software is licensed under the "3-clause ('new')
--- BSD License".  Please see the file COPYING provided with
--- this distribution for license terms.

module Data.Soundex (soundexCodes, soundex,
                     soundexSimple, soundexNARA)
where

import Data.List
import Data.Char
import Data.Array.IArray
import Data.Maybe

-- | Array of soundex codes for single characters.  The
-- array maps uppercase letters (only) to a character
-- representing a code in the range ['1'..'7'] or '?'.  Code
-- '7' is returned as a coding convenience for
-- American/Miracode/NARA/Knuth soundex.

soundexCodes :: Array Char Char
soundexCodes = accumArray updater '?' ('A', 'Z') codes where
    updater '?' c = c
    updater _ c = error ("updater called twice on " ++ [c])
    groups = [('1', "BFPV"),
              ('2', "CGJKQSXZ"),
              ('3', "DT"),
              ('4', "L"),
              ('5', "MN"),
              ('6', "R"),
              ('7', "HW")]   
    codes = concatMap make_codes groups
    make_codes (i, s) = zip s (repeat i)


-- | Utility function: id except for point substitution.
subst :: Eq a => a -> a -> a -> a
subst from to source
      | from == source = to
      | otherwise = source


-- | Compute a "full" soundex code; i.e., do not drop any
-- encodable characters from the result.  The leading
-- character of the code will be folded to uppercase.
-- Non-alphabetics are not encoded. If no alphabetics are
-- present, the soundex code will be "0".
--
-- The two commonly encountered forms of soundex are Simplified
-- and another known as American, Miracode, NARA or Knuth.  This
-- code will calculate either---passing True gets NARA, and False
-- gets Simplified.
soundex :: Bool -> String -> String
soundex nara = filter (/= '?')
             . encode
             . map toUpper
             . dropWhile (not . isAlpha)
    where
      narify
          | nara = filter (/= '7')
          | otherwise = map (subst '7' '?')
      filter_multiples = map head . group
      --- The second clause of encode originally had a bug
      --- correctly predicted by STTS (ref above)!
      encode "" = "0"
      encode as@(a : _) = (a :)
                        . drop 1
                        . filter_multiples
                        . narify
                        . map unsound $ as
      unsound c | c >= 'A' && c <= 'Z' = soundexCodes ! c
      unsound _ = '?'

soundex_truncated nara = take 4 . (++ repeat '0') . soundex nara

--- | This is the simple variant of `soundex`.  It gives the
--- first four characters of the full soundex code, zero-padded
--- as needed.
soundexSimple :: String -> String
soundexSimple = soundex_truncated False

--- | This is the most common US census variant of `soundex`,
--- compatible with most existing calculators.  It gives the
--- first four characters of the full soundex code, zero-padded
--- as needed.
soundexNARA :: String -> String
soundexNARA = soundex_truncated True

--- Some tests from the web and from Knuth that this
--- software passes.
---
-- soundexTest = and [
--    soundexSimple "Lloyd" == "L300",
--    soundexSimple "Woolcock" == "W422",
--    soundexSimple "Donnell" == "D540",
--    soundexSimple "Baragwanath" == "B625",
--    soundexSimple "Williams" == "W452",
--    soundexSimple "Ashcroft" == "A226",
--    soundexNARA   "Ashcroft" == "A261",
--    soundexSimple "Euler" == "E460",
--    soundexSimple "Ellery" == "E460",
--    soundexSimple "Gauss" == "G200",
--    soundexSimple "Ghosh" == "G200",
--    soundexSimple "Hilbert" == "H416",
--    soundexSimple "Heilbronn" == "H416",
--    soundexSimple "Knuth" == "K530",
--    soundexSimple "Kant" == "K530",
--    soundexSimple "Ladd" == "L300",
--    soundexSimple "Lukasiewicz" == "L222",
--    soundexSimple "Lissajous" == "L222"]
