module Main where

  import PGF
  import Data.List (nub, groupBy)
  import Data.Map (fromListWith, toList)
  import System.Random as R
  import Transfer (transfer)

  main :: IO ()
  main = do
    gr <- readPGF "ExtRadiology.pgf"
    putStrLn "Write your sentence here."
    putStrLn "Write quit to exit."
    loop gr (translate transfer gr)

  loop :: PGF -> (String -> [(String,String)]) -> IO ()
  loop gr trans = do
    putStr "> "
    s <- getLine
    case s of
      "quit" -> putStrLn "bye"
      "q" -> putStrLn "bye"
      "random" ->
           do smallGr <- readPGF "Radiology.pgf"
              putStrLn "How many sentences?"
              putStr "> "
              n <- readLn
              let trees = take n $ generateRandom (mkStdGen 42) smallGr (startCat smallGr)
              let trees' = [ (showExpr [] tree,
                              translateTree transfer gr tree)
                           | tree <- trees ]

              printRandomTrees trees'
              loop gr trans
      _ -> do putStrLn "Variants and translations:"
              printVariants (trans s)
              loop gr trans
    where
      printRandomTrees [] = return ()
      printRandomTrees ((tree,ss):trees_ss) = do
        putStrLn tree
        printVariants ss
        printRandomTrees trees_ss
      printVariants ss =
        mapM_ (\(l,s) -> putStrLn $ " ↳ " ++ l ++ ": " ++ s) (nub ss)
      fstEq (a,b) (a',b') = a==a'

  translate :: (Tree -> [Tree]) -> PGF -> String -> [(String,String)]
  translate tr gr s = case parseAllLang gr (startCat gr) s of
    (lg,t:_):_ -> translateTree tr gr t
    _ -> [("Error","no parse")]

  translateTree :: (Tree -> [Tree]) -> PGF -> Tree -> [(String,String)]
  translateTree tr gr t =
              [ (lang,lin)
              | l <- languages gr
              , lin <- map (linearize gr l) (tr t)
              , let lang = reverse $ take 3 $ reverse (show l)]
