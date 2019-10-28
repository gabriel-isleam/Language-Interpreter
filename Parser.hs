module Parser
where

import Util
import Data.Maybe
import InferenceDataType
import ClassState
import Data.Map as Map
import Data.Char (isUpper)

-- Program definition
type Program = Map (String, String) ClassState

initEmptyProgram :: Program
initEmptyProgram = singleton ("Global", "Global") initEmptyClass

getVars :: Program -> [[String]]
getVars program = getValues (program Map.! ("Global", "Global")) Var

getClasses :: Program -> [String]
getClasses program = Prelude.map fst (keys program)

getParentClass :: String -> Program -> String
getParentClass className program = getName className (getKeys program)

getKeys :: Program -> [(String, String)]
getKeys program = keys program

getName :: String -> [(String, String)] -> String
getName className [] = "no class"
getName className ((cName, parentName):classVector) = if cName == className then parentName
                                                      else getName className classVector

getFuncsForClass :: String -> Program -> [[String]]
getFuncsForClass [] program = []
getFuncsForClass className program 
 | classExist className program == False = []
 | otherwise = getValues (program Map.! (className, parentName)) Func
    where 
     parentName = getParentClass className program



type Instruction = [String]

specialCharacters = [' ', ',', ':', '(', ')', '=']



parse :: String -> [Instruction]
parse string = checkValidity (makeInstruction string [] []) []

-- all parameters for functions should begin with a capital letter
-- except for the third parameter which the function name
checkValidity :: [Instruction] -> [Instruction] -> [Instruction]
checkValidity [] acc = acc
checkValidity (x:instruction) acc = if (capitalLetters x 1 == True) then checkValidity instruction (acc ++ [x])
                                    else checkValidity instruction acc

capitalLetters :: [String] -> Int -> Bool
capitalLetters [] index= True
capitalLetters (x:instruction) index
 | x == "class" = True
 | x == "newvar" = True
 | isUpper (head x) == False && index /= 3 = False
 | otherwise = capitalLetters instruction (index + 1)

-- removes special characters
removeSpaces :: String -> String
removeSpaces [] = []
removeSpaces s@(x:string) = if (elem x specialCharacters) then removeSpaces string
                            else s

-- verifies if the first character is \n (endline)
checkEndLine :: String -> Bool
checkEndLine (x:string) = if (x == '\n') then True
                          else False

-- takes one word from a line
takeWord :: String -> String -> String
takeWord [] acc = Prelude.reverse acc
takeWord (x:string) acc = if (elem x specialCharacters == False) then takeWord string (x:acc)
                          else (Prelude.reverse acc)

removeFirstWord :: String -> String
removeFirstWord [] = []
removeFirstWord (x:string) = if (elem x specialCharacters) then removeSpaces string
                             else removeFirstWord string

-- function that takes each line and transforms it into an instruction
takeSequence :: String -> [String] -> [String]
takeSequence string acc = if (word /= [] && (checkEndLine string == False)) then takeSequence (removeFirstWord string) (word:acc)
                          else Prelude.reverse acc
    where
     word = takeWord string []

-- function that performs the string of instructions
makeInstruction :: String -> String -> [[String]] -> [[String]]
makeInstruction [] acc sequence = sequence ++ [takeSequence acc []]
makeInstruction (x:string) acc sequence 
 | x /= '\n' = makeInstruction string (acc ++ [x]) sequence
 | acc /= [] = makeInstruction string [] (sequence ++ [takeSequence acc []])
 | otherwise = makeInstruction string [] sequence


interpret :: Instruction -> Program -> Program
interpret [] program = program 
interpret instr program
 | head instr == "class" && length instr <= 2 && classExist (instr !! 1) program == False = Map.insert (last instr, "Global") initEmptyClass program
 | head instr == "class" && length instr > 2 && classExist (instr !! 1) program == False = Map.insert (instr !! 1,  parentClass) initEmptyClass program
 | head instr == "newvar" && classExist className program == True = Map.insert ("Global", "Global") newVars program                                
 | isUpper (head (head instr)) && classExist cNF program && checkParam (drop 3 instr) program && classExist (instr !! 0) program = Map.insert (cNF, parentName) newFuncs program
 | otherwise = program
    where
     --variables for "newvar" case
     className = instr !! 2
     varName = instr !! 1
     oldVars = program Map.! ("Global","Global")
     newVars = insertIntoClass oldVars Var [varName, className]
     --variables for "func" case
     cNF@classNameFunc = instr !! 1
     parentName = getParentClass classNameFunc program
     oldFuncs = program Map.! (classNameFunc, parentName)
     newFuncs = insertIntoClass oldFuncs Func (createFunctionString instr (length instr) [])
     --parent class name for "class extends" case
     parentClass = if (classExist (instr !! 3) program == False) then "Global"
                   else (instr !! 3)

-- creates the appropriate string for the function to be stored
createFunctionString :: [String] -> Int -> [String] -> [String]
createFunctionString instruction 3 acc = (instruction !! 2) : (instruction !! 0) : acc
createFunctionString instruction size acc = createFunctionString instruction (size - 1) (instruction !! (size - 1) : acc)

-- check the parameters for a function, to have the required syntax
checkParam :: [String] -> Program -> Bool
checkParam [] program = True
checkParam (x:instruction) program = if (classExist x program == False) then False
                                          else checkParam instruction program

-- check if the class exists in the program
classExist :: String -> Program -> Bool
classExist className program = if (elem className (getClasses program)) then True
                               else False

infer :: Expr -> Program -> Maybe String
infer = undefined


