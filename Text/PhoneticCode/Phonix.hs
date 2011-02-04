--- Phonix code calculator
--- Copyright © 2008 Bart Massey
--- ALL RIGHTS RESERVED

--- This software is licensed under the "3-clause ('new')
--- BSD License".  Please see the file COPYING provided with
--- this distribution for license terms.

-- | Phonix codes (Gadd 1990) augment slightly improved
-- Soundex codes with a preprocessing step for cleaning up
-- certain n-grams.  Since the preprocessing step contains
-- around 90 rules processed by a slow custom-written
-- scanner, this implementation is not too fast.
-- 
-- This code was based on a number of sources, including the
-- CPAN Phonix code calculator `Text::Phonetic::Phonix.pm`. 
-- Because the paper describing the codes is not freely
-- available and I'm lazy, I did not use it as a reference.
-- Also because Phonix involves around 90 substitution rules,
-- I transformed the Perl ones, which was easier than
-- generating them from scratch.

module Text.PhoneticCode.Phonix (
  phonix, phonixCodes, 
  phonixRules, applyPhonixRules )
where

import Data.List
import Data.Char
import Data.Array.IArray
import qualified Data.Set as Set
import Text.Regex

-- | Compute a "full" phonix code; i.e., do not drop any
-- encodable characters from the result.  The leading
-- character of the code will be folded to uppercase.
-- Non-alphabetics are not encoded. If no alphabetics are
-- present, the phonix code will be "0".
--
-- There appear to be many, many variants of phonix
-- implemented on the web, and I'm too cheap and lazy to go
-- find the original paper by Gadd (1990) that actually
-- describes the original algorithm.  Thus, I am taking some
-- big guesses on intent here as I implement.  Corrections,
-- especially those involving getting me a copy of the
-- article, are welcome.
--
-- Dropping the "trailing sound" seems to be an integral
-- part of Gadd's technique, but I'm not sure how it is
-- supposed to be done.  I am currently compressing runs of
-- vowels, and then dropping the trailing digit or vowel
-- from the code.
--
-- Another area of confusion is whether to compress strings
-- of the same code, as in Soundex, or merely strings of the
-- same consonant.  I have chosen the former.
phonix :: String -> String
phonix = filter (/= '?')
       . drop_trailing_sound
       . encode
       . applyPhonixRules
    where
      drop_trailing_sound = init . concatMap question_squash . group where
          question_squash ('?' : _) = ['?']
          question_squash l = l
      filter_multiples = map head . group
      encode "" = "0"
      encode as@(a : _) = (devowel a :)
                        . drop 1
                        . filter_multiples
                        . map unsound $ as
      unsound c | c >= 'A' && c <= 'Z' = phonixCodes ! c
      unsound _ = '?'
      devowel c | isVowely c = 'v'
      devowel c = c

isVowely :: Char -> Bool
isVowely c = c `Set.member` (Set.fromList "AEIOUY")

-- | Array of phonix codes for single characters.  The
-- array maps uppercase letters (only) to a character
-- representing a code in the range ['1'..'8'] or '?'.
phonixCodes :: Array Char Char
phonixCodes = accumArray updater '?' ('A', 'Z') codes where
    updater '?' c = c
    updater _ c = error ("updater called twice on " ++ [c])
    groups = [('1', "BP"),
              ('2', "CGJKQ"),
              ('3', "DT"),
              ('4', "L"),
              ('5', "MN"),
              ('6', "R"),
              ('7', "FV"),
              ('8', "SXZ")]   
    codes = concatMap make_codes groups
    make_codes (i, s) = zip s (repeat i)

-- | Substitution rules for Phonix canonicalization.  "^" ("$")
-- is used to anchor a pattern to the beginning (end) of the word.
-- "c" ("v", ".") at the beginning or end of a pattern match
-- a consonant (vowel, arbitrary character).  A character matched
-- in this fashion is automatically tacked onto the beginning (end)
-- of the pattern.
phonixRules :: [(String, String)]
phonixRules = [
 ("DG","G"),
 ("CO","KO"),
 ("CA","KA"),
 ("CU","KU"),
 ("CY","SI"),
 ("CI","SI"),
 ("CE","SE"),
 ("^CLv","KL"),
 ("CK","K"),
 ("GC$","K"),
 ("JC$","K"),
 ("^CRv","KR"),
 ("^CHRv","KR"),
 ("^WR","R"),
 ("NC","NK"),
 ("CT","KT"),
 ("PH","F"),
 ("AA","AR"),  --- neu
 ("SCH","SH"),
 ("BTL","TL"),
 ("GHT","T"),
 ("AUGH","ARF"),
 (".LJv","LD"),
 ("LOUGH","LOW"),
 ("^Q","KW"),
 ("^KN","N"),
 ("GN$","N"),
 ("GHN","N"),
 ("GNE$","N"),
 ("GHNE","NE"),
 ("GNES$","NS"),
 ("^GN","N"),
 (".GNc","N"),
 ("^PS","S"),
 ("^PT","T"),
 ("^CZ","C"),
 ("vWZ.","Z"),
 (".CZ.","CH"),
 ("LZ","LSH"),
 ("RZ","RSH"),
 (".Zv","S"),
 ("ZZ","TS"),
 ("cZ.","TS"),
 ("HROUGH","REW"),
 ("OUGH","OF"),
 ("vQv","KW"),
 ("vJv","Y"),
 ("^YJv","Y"),
 ("^GH","G"),
 ("vGH$","E"),
 ("^CY","S"),
 ("NX","NKS"),
 ("^PF","F"),
 ("DT$","T"),
 ("TL$","TIL"),
 ("DL$","DIL"),
 ("YTH","ITH"),
 ("^TJv","CH"),
 ("^TSJv","CH"),
 ("^TSv","T"),
 ("TCH","CH"),   --- old che
 ("^vWSK","VSIKE"),
 ("^PNv","N"),
 ("^MNv","N"),
 ("vSTL","SL"),
 ("TNT$","ENT"),
 ("EAUX$","OH"),
 ("EXCI","ECS"),
 ("X","ECS"),
 ("NED$","ND"),
 ("JR","DR"),
 ("EE$","EA"),
 ("ZS","S"),
 ("vRc","AH"),
 ("vHRc","AH"),
 ("vHR$","AH"),
 ("RE$","AR"),
 ("vR$","AH"),
 ("LLE","LE"),
 ("cLE$","ILE"),
 ("cLES$","ILES"),
 ("E$",""),
 ("ES$","S"),
 ("vSS","AS"),
 ("vMB$","M"),
 ("MPTS","MPS"),
 ("MPS","MS"),
 ("MPT","MT") ]

-- | Apply each of the Phonix preprocessing rules in turn to
-- the target word returning the resulting accumulated
-- substitution.
applyPhonixRules :: String -> String
applyPhonixRules = 
  flip (foldl' res) phonixRulesREs .
    map toUpper . 
    dropWhile (not . isAlpha)
  where
    res target (pat, subst) = subRegex pat target subst

-- List of pattern/substitution pairs built from the
-- phonixRules .
phonixRulesREs :: [(Regex, String)]
phonixRulesREs =
  map reFormat phonixRules
  where
    mre s = mkRegexWithOpts s False True
    reFormat (src, dst) =
      let vowelSubst = mre "v"
          consSubst = mre "c"
          dotSubst = mre "\\." in
      let src' = flip (subRegex dotSubst) "\\([A-Z]\\)" $
                 flip (subRegex consSubst) "\\([BCDFGHJKLMNPQRSTVWXYZ]\\)" $
                 flip (subRegex vowelSubst) "\\([AEIOU]\\)" $
                 src in
      let front = 
            case matchRegex (mre "^\\^?[^^A-Z]") src' of
              Just _ -> "\\1"
              Nothing -> ""
              in
      let back = 
            case matchRegex (mre "[^A-Z$]\\$?$") src' of
              Just _ -> 
                case front of
                  "" -> "\\1"
                  _ -> "\\2"
              Nothing -> ""
              in
      (mre src', front ++ dst ++ back)
