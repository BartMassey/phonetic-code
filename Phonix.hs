--- Phonix code calculator
--- Copyright © 2008 Bart Massey
--- ALL RIGHTS RESERVED

--- Based on the CPAN Phonix code calculator
--- Text::Phonetic::Phonix.pm, because the paper describing
--- the codes is not freely available and I'm lazy.
--- Also because Phonix involves about 150 substitution rules,
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
(DG,"G"),
(C([OAU]),"K1"),
(C[YI],"SI"),
(CE,"SE"),
(^CL([AEIOU]),"KL1"),
(CK,"K"),
([GJ]C$,"K"),
(^CH?R([AEIOU]),"KR1"),
(^WR,"R"),
(NC,"NK"),
(CT,"KT"),
(PH,"F"),
(AA,"AR"], #neu
(SCH,"SH"),
(BTL,"TL"),
(GHT,"T"),
(AUGH,"ARF"),
((\w)LJ([AEIOU]),"1LD2"),
(LOUGH,"LOW"),
(^Q,"KW"),
(^KN,"N"),
(GN$,"N"),
(GHN,"N"),
(GNE$,"N"),
(GHNE,"NE"),
(GNES$,"NS"),
(^GN,"N"),
((\w)GN([BCDFGHJLMNPQRSTVXZXY])v,"1N2"),
(^PS,"S"),
(^PT,"T"),
(^CZ,"C"),
(([AEIOU])WZ(\w),"1Z2"),
((\w)CZ(\w),"1CH2"),
(LZ,"LSH"),
(RZ,"RSH"),
((\w)Z([AEIOU]),"1S2"),
(ZZ,"TS"),
(([BCDFGHJLMNPQRSTVXZXY])Z(\w),"1TS2"),
(HROUGH,"REW"),
(OUGH,"OF"),
(([AEIOU])Q([AEIOU]),"1KW2"),
(([AEIOU])Q([AEIOU]),"1Y2"),
(^YJ([AEIOU]),"Y1"),
(^GH,"G"),
(([AEIOU])GH$,"1E"),
(^CY,"S"),
(NX,"NKS"),
(^PF,"F"),
(DT$,"T"),
((T|D)L$,"1IL"),
(YTH,"ITH"),
(^TS?J([AEIOU]),"CH1"),
(^TS([AEIOU]),"T1"),
(TCH,"CH"], # old che
(^([AEIOU])WSK,"1VSIKE"),
(^[PM]N([AEIOU]),"N1"),
(([AEIOU])STL,"1SL"),
(TNT$,"ENT"),
(EAUX$,"OH"),
(EXCI,"ECS"),
(X,"ECS"),
(NED$,"ND"),
(JR,"DR"),
(EE$,"EA"),
(ZS,"S"),
(([AEIOU])H?R([BCDFGHJLMNPQRSTVXZXY]),"1AH2"),
(([AEIOU])HR$,"1AH"),
(RE$,"AR"),
(([AEIOU])R$,"1AH"),
(LLE,"LE"),
(([BCDFGHJLMNPQRSTVXZXY])LE(S?)$,"1ILE2"),
(E$,""),
(ES$,"S"),
(([AEIOU])SS,"1AS"),
(([AEIOU])MB$,"1M"),
(MPTS,"MPS"),
(MPS,"MS"),
(MPT,"MT")]

