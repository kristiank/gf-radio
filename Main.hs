module Main where

  import PGF
  import Data.List (nub, groupBy)
  import Data.Map (fromListWith, toList)
  import System.Random as R
  import ExtRadiology

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

  transfer :: Tree -> [Tree]
  transfer = map gf . normalize funs . fg
   where funs = [aggregateDescn, verboseDescn, changePolarityDs]

----------------------------------------------------
-- Transfer functions

  normalize :: [GDescriptions -> GDescriptions] -> GStatement -> [GStatement]
  normalize fs st = case st of
    GPred org descn ->
      [GPred org (f descn) | f <- fs] ++
      [GPredAdv org (f descn) | f <- fs]
    GPredAdv org descn ->
      [GPred org (f descn) | f <- fs] ++
      [GPredAdv org (f descn) | f <- fs]

  aggregateDescn :: GDescriptions -> GDescriptions
  aggregateDescn descn = case descn of
    GDesc2 dn1 dn2 -> aggregateDescn2 dn1 dn2
    GDesc3 dn1 dn2 dn3 -> aggregateDescn3 dn1 dn2 dn3
    _ -> descn
    where
      aggregateDescn2 :: GDescription -> GDescription -> GDescriptions
      aggregateDescn2 dn1@(GDescribePos dr1 p1 proof1) dn2@(GDescribePos dr2 p2 proof2)
        | show dr1 == show dr2 && show p1 == show p2 = GDesc1 dn1
        | show dr1 == show dr2 = GDesc1 (GAggregateProperty2Pos dr1 p1 p2 proof1 proof2)
        | show p1  == show p2  = GDesc1 (GAggregateDescriptor2Pos p1 dr1 dr2 proof1 proof2)
        | otherwise = GDesc2 dn1 dn2
      aggregateDescn2 dn1@(GDescribeNeg dr1 p1 proof1) dn2@(GDescribeNeg dr2 p2 proof2)
        | show dr1 == show dr2 && show p1 == show p2 = GDesc1 dn1
        | show dr1 == show dr2 = GDesc1 (GAggregateProperty2Neg dr1 p1 p2 proof1 proof2)
        | show p1  == show p2  = GDesc1 (GAggregateDescriptor2Neg p1 dr1 dr2 proof1 proof2)
        | otherwise = GDesc2 dn1 dn2
      aggregateDescn2 dn1 dn2 = GDesc2 dn1 dn2

      aggregateDescn3 :: GDescription -> GDescription -> GDescription -> GDescriptions
      aggregateDescn3 dn1@(GDescribePos dr1 p1 proof1) dn2@(GDescribePos dr2 p2 proof2) dn3@(GDescribePos dr3 p3 proof3)
        | show dr1 == show dr2 && show p1 == show p2 = GDesc2 dn1 dn3
        | show dr1 == show dr3 && show p1 == show p3 = GDesc2 dn1 dn2
        | show dr2 == show dr3 && show p2 == show p3 = GDesc2 dn1 dn2
        | show dr1 == show dr2 = GDesc2 (GAggregateProperty2Pos dr1 p1 p2 proof1 proof2) dn3
        | show dr1 == show dr3 = GDesc2 (GAggregateProperty2Pos dr1 p1 p3 proof1 proof3) dn2
        | show dr2 == show dr3 = GDesc2 (GAggregateProperty2Pos dr2 p2 p3 proof2 proof3) dn1
        | show p1 == show p2 = GDesc2 (GAggregateDescriptor2Pos p1 dr1 dr2 proof1 proof2) dn3
        | show p1 == show p3 = GDesc2 (GAggregateDescriptor2Pos p1 dr1 dr3 proof1 proof3) dn2
        | show p2 == show p3 = GDesc2 (GAggregateDescriptor2Pos p2 dr2 dr3 proof2 proof3) dn1
        | otherwise = GDesc3 dn1 dn2 dn3
      aggregateDescn3 dn1@(GDescribeNeg dr1 p1 proof1) dn2@(GDescribeNeg dr2 p2 proof2) dn3@(GDescribeNeg dr3 p3 proof3)
        | show dr1 == show dr2 && show p1 == show p2 = GDesc2 dn1 dn3
        | show dr1 == show dr3 && show p1 == show p3 = GDesc2 dn1 dn2
        | show dr2 == show dr3 && show p2 == show p3 = GDesc2 dn1 dn2
        | show dr1 == show dr2 = GDesc2 (GAggregateProperty2Neg dr1 p1 p2 proof1 proof2) dn3
        | show dr1 == show dr3 = GDesc2 (GAggregateProperty2Neg dr1 p1 p3 proof1 proof3) dn2
        | show dr2 == show dr3 = GDesc2 (GAggregateProperty2Neg dr1 p2 p3 proof2 proof3) dn1
        | show p1 == show p2 = GDesc2 (GAggregateDescriptor2Neg p1 dr1 dr2 proof1 proof2) dn3
        | show p1 == show p3 = GDesc2 (GAggregateDescriptor2Neg p1 dr1 dr3 proof1 proof3) dn2
        | show p2 == show p3 = GDesc2 (GAggregateDescriptor2Neg p2 dr2 dr3 proof2 proof3) dn1
        | otherwise = GDesc3 dn1 dn2 dn3
      aggregateDescn3 dn1 dn2 dn3 = GDesc3 dn1 dn2 dn3

  verboseDescn :: GDescriptions -> GDescriptions
  verboseDescn (GDesc1 descn) = case descn of
    GAggregateProperty2Pos descr p1 p2 proof1 proof2 -> GDesc2 (GDescribePos descr p1 proof1) (GDescribePos descr p2 proof2)
    GAggregateProperty2Neg descr p1 p2 proof1 proof2 -> GDesc2 (GDescribeNeg descr p1 proof1) (GDescribeNeg descr p2 proof2)
    -- GDesc2 d1 d2 -> GDesc2 (verboseDescn d1) (verboseDescn d2)
    -- GDesc3 d1 d2 d3 -> GDesc3 (verboseDescn d1) (verboseDescn d2) (verboseDescn d3)
    _ -> GDesc1 descn
  verboseDescn descn = descn

  changePolarityDs :: GDescriptions -> GDescriptions
  changePolarityDs descns = case descns of
    GDesc1 d -> GDesc1 (changePolarity d)
    GDesc2 d1 d2 -> GDesc2 (changePolarity d1) (changePolarity d2)
    GDesc3 d1 d2 d3 -> GDesc3 (changePolarity d1) (changePolarity d2) (changePolarity d3)

  changePolarity :: GDescription -> GDescription
  changePolarity descn = case descn of
    GAggregateProperty2Pos dr p1 p2 proof1 proof2 ->
      case dr of
        GAbnormal -> GAggregateProperty2Neg GNormal p1 p2 proof1 proof2
        GNormal -> GAggregateProperty2Neg GAbnormal p1 p2 proof1 proof2
        _ -> descn
    GAggregateProperty2Neg dr p1 p2 proof1 proof2 ->
      case dr of
        GAbnormal -> GAggregateProperty2Pos GNormal p1 p2 proof1 proof2
        GNormal -> GAggregateProperty2Pos GAbnormal p1 p2 proof1 proof2
        _ -> descn
    GDescribePos dr p proof ->
      case dr of
        GAbnormal -> GDescribeNeg GNormal p proof
        GNormal -> GDescribeNeg GAbnormal p proof
        _ -> descn
    GDescribeNeg dr p proof ->
      case dr of
        GAbnormal -> GDescribePos GNormal p proof
        GNormal -> GDescribePos GAbnormal p proof
        _ -> descn
