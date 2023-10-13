{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.List
import qualified Data.Text as T
import qualified Data.Text.Read as T
import Numeric (readSigned)

main :: IO ()
main = putStrLn "Hello, Haskell!"

data Line
    = CdTop -- $ cd /
    | CdUp -- $ cd ..
    | CdDown T.Text -- $ cd <folder> 
    | Ls -- $ ls
    | LsDir -- dir <name>
    | LsFile Int -- <size> <name>
    deriving (Eq, Show)

parseLine :: T.Text -> Line
parseLine xs =
    case T.words xs of
        ["$", "cd", "/"] -> CdTop
        ["$", "cd", ".."] -> CdUp
        ["$", "cd", name] -> CdDown name
        ["$", "ls"] -> Ls
        ["dir", _] -> LsDir
        [size, _] -> LsFile $ toInt size

toInt :: T.Text -> Int
toInt xs =
    case T.decimal xs of
        Left _ -> 0
        Right (x, _) -> x

parse :: String -> [Line]
parse = map parseLine . T.lines . T.pack

stripLs :: [Line] -> [Line]
stripLs = filter (/= Ls) . filter (/= LsDir)

makeFileSystem xs = 
  let input = stripLs . parse $ xs
      initialTree = Folder "/" []
      initialState :: State
      initialState = (initialTree, ["/"]) in
      foldl (flip doLine) initialState input

data Tree
    = Folder T.Text [Tree]
    | File Int
    deriving (Eq, Show)

type State = (Tree, [T.Text])

newFolder :: T.Text -> Tree
newFolder = flip Folder []

treeAppend :: State -> Tree -> Tree
treeAppend (Folder name files, [curFolder]) newFolderName =
    Folder name (newFolderName : files) -- TODO: don't insert if already exists
treeAppend (Folder curFolderName curFolderFiles, _:nextFolder:path) newFolderName =
    let ([next], folders) = partition isNextFolder curFolderFiles
        newF = treeAppend (next, nextFolder : path) newFolderName
     in Folder curFolderName (newF : folders)
  where
    isNextFolder (File _) = False
    isNextFolder (Folder name _) = name == nextFolder

doCdDown :: State -> T.Text -> State
doCdDown state@(_, oldPath) newFolderName =
    let newTree = treeAppend state (newFolder newFolderName)
     in (newTree, oldPath ++ [newFolderName])

doFile :: State -> Int -> State
doFile state@(_, oldPath) newFileSize =
    let newTree = treeAppend state (File newFileSize)
     in (newTree, oldPath)

doLine :: Line -> State -> State
doLine CdTop (tree, _) = (tree, ["/"])
doLine CdUp (tree, path) = (tree, init path)
doLine (CdDown folder) (tree, path) = doCdDown (tree, path) folder
doLine (LsFile fileSize) (tree, path) = doFile (tree, path) fileSize
