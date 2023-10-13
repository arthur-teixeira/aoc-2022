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

data Tree
  = Folder T.Text [Tree]
  | File Int
  deriving (Eq, Show)

a =
  Folder "/" [Folder "asdf" [File 123, File 234], Folder "ahbdfg" [], File 235]

--createFolder "newFolder" (a, ["/", "asdf"])
b =
  Folder
    "/"
    [ Folder "asdf" [File 123, File 234, Folder "newFolder" []]
    , Folder "ahbdfg" []
    , File 235
    ]

-- Hold the node "/" as the tree and a list of paths to the current folder, like
-- (Tree, [T.Text]) as a state, take a command and execute it on the tree
type State = (Tree, [T.Text])

newFolder :: T.Text -> Tree
newFolder name = Folder name []

insertFolder :: State -> Tree -> Tree
insertFolder (Folder name files, [curFolder]) newFolder =
  Folder name (newFolder : files)

insertFolder (Folder curFolderName curFolderFiles, curFolder:nextFolder:path) newFolder =
  let ([next], folders) = partition isNextFolder curFolderFiles
      newF = insertFolder (next, nextFolder : path) newFolder
   in Folder curFolderName (newF : folders)
  where
    isNextFolder (File _) = False
    isNextFolder (Folder name _) = name == nextFolder

doLine :: Line -> State -> State
doLine CdTop (tree, _) = (tree, ["/"])
doLine CdUp (tree, path) = (tree, init path)
doLine (CdDown folder) (tree, path) = (tree, path ++ [folder])
