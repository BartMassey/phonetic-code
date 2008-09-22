--- Phonix code calculator
--- Copyright © 2008 Bart Massey
--- ALL RIGHTS RESERVED

--- This software is licensed under the "3-clause ('new')
--- BSD License".  Please see the file COPYING provided with
--- this distribution for license terms.

-- |Phonix codes (Gadd 1990) augment slightly improved Soundex codes
--  with a preprocessing step for cleaning up certain n-grams.  Since
--  the preprocessing step contains more than 150 rules processed by
--  a slow custom-written scanner, this implementation is not too fast.
-- 
-- This code was based on a number of sources, including
-- the CPAN Phonix code calculator
-- Text::Phonetic::Phonix.pm . Because the paper describing
-- the codes is not freely available and I'm lazy, I did not use
-- it as a reference.
-- Also because Phonix involves over 150 substitution rules,
-- I transformed the Perl ones, which was easier than
-- generating them from scratch.

module Text.PhoneticCode.Phonix (phonix, phonixCodes, phonixRules)
where

import Data.List
import Data.Char
import Data.Array.IArray
import Data.Maybe
import qualified Data.Set as Set


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
-- big guesses on intent here as I implement.
-- Corrections, especially those involving getting me a copy
-- of the article, are welcome.
--
-- Dropping the "trailing sound" seems to be
-- an integral part of Gadd's technique, but I'm not sure how
-- it is supposed to be done.  I am currently compressing runs
-- of vowels, and then dropping the trailing digit or vowel
-- from the code.
--
-- Another area of confusion is whether to compress strings of
-- the same code, as in Soundex, or merely strings of the same
-- consonant.  I have chosen the former.
phonix :: String -> String
phonix = filter (/= '?')
       . drop_trailing_sound
       . encode
       . apply_rules
       . map toUpper
       . dropWhile (not . isAlpha)
    where
      drop_trailing_sound = init . concatMap question_squash . group where
          question_squash ('?' : _) = ['?']
          question_squash l = l
      apply_rules w = foldl' (flip $ uncurry gSubst) w phonixRules
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

isVowel :: Char -> Bool
isVowel c = c `Set.member` (Set.fromList "AEIOU")

globMatches :: Char -> Char -> Bool
globMatches 'v' = isVowel
globMatches 'c' = not . isVowel
globMatches '.' = const True
globMatches _ = const False

isGlob :: Char -> Bool
isGlob c = c `Set.member` (Set.fromList "vc.")

matches :: Char -> Char -> Bool
s `matches` t = s == t || s `globMatches` t

subst :: String -> String -> String -> String
subst "" _ _ = error "subst: bad pattern"
subst _ _ "" = ""
subst s d t
    | length t < length s = t
    | and (zipWith matches s t) = deglob ++ subst s d t'
    | otherwise = skip
    where
      deglob = dn . d1 $ d where
          d1 d0
              | isGlob . head $ s = head t : d0
              | otherwise = d0
          dn d0
              | isGlob . last $ s = d0 ++ [t !! (length s - 1)]
              | otherwise = d0
      t' = drop (length s) t
      skip = head t : subst s d (tail t)

gSubst :: String -> String -> String -> String
gSubst "" _ _ = error "gSubst: bad pattern"
gSubst s d t
    | head s == '^' = let (t0, t1) = splitAt (length s - 1) t in
                      subst (tail s) d t0 ++ t1
    | last s == '$' = let (t0, t1) = splitAt (length t - length s + 1) t in
                      t0 ++ subst (init s) d t1
    | otherwise = subst s d t


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
