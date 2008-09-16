--- Phonix code calculator
--- Copyright © 2008 Bart Massey
--- ALL RIGHTS RESERVED

--- Based on the CPAN Phonix code calculator
--- Text::Phonetic::Phonix.pm, because the paper describing
--- the codes is not freely available and I'm lazy.
--- Also because Phonix involves over 150 substitution rules,
--- I transformed the Perl ones, which was easier than
--- generating them from scratch.

--- This software is licensed under the "3-clause ('new')
--- BSD License".  Please see the file COPYING provided with
--- this distribution for license terms.

module Text.PhoneticCode.Phonix
where

import Data.List
import Data.Char
import Data.Array.IArray
import Data.Maybe

-- | Array of phonix codes for single characters.  The
-- array maps uppercase letters (only) to a character
-- representing a code in the range ['1'..'7'] or '?'.

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




-- | Compute a "full" phonix code; i.e., do not drop any
-- encodable characters from the result.  The leading
-- character of the code will be folded to uppercase.
-- Non-alphabetics are not encoded. If no alphabetics are
-- present, the phonix code will be "0".
--- phonix :: Bool -> String -> String

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
 (".GNc","N"),  --- the Perl rule was apparently typoed
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
-- ("vQv","Y"),   --- this rule apparently typoed, but how?
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
