module Text.EditDistance
where

data EditCosts = EditCosts { editCostInsert :: Int,
                             editCostDelete :: Int,
                             editCostReplace :: Int }

editDistance' cost s t = edit (length s) (length t) where
    edit 0 0 = 0
    edit 0 j = j * editCostInsert cost
    edit i 0 = i * editCostDelete cost
    edit i j = minimum [ edit (i - 1) j + editCostDelete cost,
                         edit i (j - 1) + editCostInsert cost,
                         edit (i - 1) (j - 1) + r i j ]
    r i j
        | (s !! (i - 1)) == (t !! (j - 1)) = 0
        | otherwise = editCostReplace cost

stdEditCosts = EditCosts { editCostInsert = 1,
                           editCostDelete = 1,
                           editCostReplace = 1 }

expEditCosts = EditCosts { editCostInsert = 10,
                           editCostDelete = 100,
                           editCostReplace = 1 }

ed s t = editDistance' expEditCosts s t

editDistance s t = editDistance' stdEditCosts s t
