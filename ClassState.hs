module ClassState
where

import Data.Map as Map

-- used for obtaining informations about variables and functions
data InstrType = Var | Func  deriving (Show, Eq, Ord)

type ClassState = Map InstrType [[String]]

initEmptyClass :: ClassState
initEmptyClass = Map.empty

insertIntoClass :: ClassState -> InstrType -> [String] -> ClassState
insertIntoClass classe k v = Map.insertWith (++) k [v] classe


getValues :: ClassState -> InstrType -> [[String]]
getValues classe k = if Map.member k classe == True
                        then classe Map.! k
                        else [] 
