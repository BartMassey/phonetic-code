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

module Data.Phonix (phonixCodes, phonix)
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
phonix :: Bool -> String -> String

phonixRules :: [(String, String)]
phonixRules = [
("DG","G"),
("CO","KO"),
("CA","KA"),
("CU","KU"),
("CY","SI"),
("CI","SI"),
("CE","SE"),
("^CLv","KL1"),
("CK","K"),
("GC$","K"),
("JC$","K"),
("^CRv","KR1"),
("^CHRv","KR1"),
("^WR","R"),
("NC","NK"),
("CT","KT"),
("PH","F"),
("AA","AR"]",  --- neu
("SCH","SH"),
("BTL","TL"),
("GHT","T"),
("AUGH","ARF"),
(".LJv","1LD2"),
("LOUGH","LOW"),
("^Q","KW"),
("^KN","N"),
("GN$","N"),
("GHN","N"),
("GNE$","N"),
("GHNE","NE"),
("GNES$","NS"),
("^GN","N"),
(".GNc","1N2"),  --- the Perl rule was apparently typoed
("^PS","S"),
("^PT","T"),
("^CZ","C"),
("vWZ.","1Z2"),
(".CZ.","1CH2"),
("LZ","LSH"),
("RZ","RSH"),
(".Zv","1S2"),
("ZZ","TS"),
("cZ.","1TS2"),
("HROUGH","REW"),
("OUGH","OF"),
("vQv","1KW2"),
("vQv","1Y2"),
("^YJv","Y1"),
("^GH","G"),
("vGH$","1E"),
("^CY","S"),
("NX","NKS"),
("^PF","F"),
("DT$","T"),
("TL$","TIL"),
("DL$","DIL"),
("YTH","ITH"),
("^TJv","CH1"),
("^TSJv","CH1"),
("^TSv","T1"),
("TCH","CH"]",   --- old che
("^vWSK","1VSIKE"),
("^PNv","N1"),
("^MNv","N1"),
("vSTL","1SL"),
("TNT$","ENT"),
("EAUX$","OH"),
("EXCI","ECS"),
("X","ECS"),
("NED$","ND"),
("JR","DR"),
("EE$","EA"),
("ZS","S"),
("vRc","1AH2"),
("vHRc","1AH2"),
("vHR$","1AH"),
("RE$","AR"),
("vR$","1AH"),
("LLE","LE"),
("cLE$","1ILE"),
("cLES$","1ILES"),
("E$",""),
("ES$","S"),
("vSS","1AS"),
("vMB$","1M"),
("MPTS","MPS"),
("MPS","MS"),
("MPT","MT")]
