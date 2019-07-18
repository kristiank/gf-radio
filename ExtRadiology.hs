module ExtRadiology where

import PGF hiding (Tree)
----------------------------------------------------
-- automatic translation from GF to Haskell
----------------------------------------------------

class Gf a where
  gf :: a -> Expr
  fg :: Expr -> a

newtype GString = GString String  deriving Show

instance Gf GString where
  gf (GString x) = mkStr x
  fg t =
    case unStr t of
      Just x  ->  GString x
      Nothing -> error ("no GString " ++ show t)

newtype GInt = GInt Int  deriving Show

instance Gf GInt where
  gf (GInt x) = mkInt x
  fg t =
    case unInt t of
      Just x  ->  GInt x
      Nothing -> error ("no GInt " ++ show t)

newtype GFloat = GFloat Double  deriving Show

instance Gf GFloat where
  gf (GFloat x) = mkFloat x
  fg t =
    case unFloat t of
      Just x  ->  GFloat x
      Nothing -> error ("no GFloat " ++ show t)

----------------------------------------------------
-- below this line machine-generated
----------------------------------------------------

data GDescribes =
   GAbnormal_Location 
 | GAbnormal_Size 
 | GExternal_Location 
 | GLateral_Location 
 | GMicroscopic_Size 
 | GNormal_Location 
 | GNormal_Size 
 | GSmall_Size 
 | GWidened_Size 
 | Gmm_Size 
 | Gmm_lessThan_Size 
  deriving Show

data GDescription =
   GAggregateDescriptor2Neg GProperty GDescriptor GDescriptor GDescribes GDescribes 
 | GAggregateDescriptor2Pos GProperty GDescriptor GDescriptor GDescribes GDescribes 
 | GAggregateProperty2Neg GDescriptor GProperty GProperty GDescribes GDescribes 
 | GAggregateProperty2Pos GDescriptor GProperty GProperty GDescribes GDescribes 
 | GDescribeNeg GDescriptor GProperty GDescribes 
 | GDescribePos GDescriptor GProperty GDescribes 
  deriving Show

data GDescriptions =
   GDesc1 GDescription 
 | GDesc2 GDescription GDescription 
 | GDesc3 GDescription GDescription GDescription 
  deriving Show

data GDescriptor =
   GAbnormal 
 | GExternal 
 | GLateral 
 | GMicroscopic 
 | GNormal 
 | GSmall 
 | GWidened 
 | Gmm GNumber 
 | GmmLessThan GNumber 
  deriving Show

data GDualOrgan =
   GKidney 
 | GOvary 
  deriving Show

data GNumber = GNum20 
  deriving Show

data GOrgan =
   GBoth GDualOrgan 
 | GHeart 
 | GLeft GDualOrgan 
 | GRight GDualOrgan 
 | GSpleen 
  deriving Show

data GProperty =
   GLocation 
 | GSize 
  deriving Show

data GStatement =
   GPred GOrgan GDescriptions 
 | GPredAdv GOrgan GDescriptions 
  deriving Show


instance Gf GDescribes where
  gf GAbnormal_Location = mkApp (mkCId "Abnormal_Location") []
  gf GAbnormal_Size = mkApp (mkCId "Abnormal_Size") []
  gf GExternal_Location = mkApp (mkCId "External_Location") []
  gf GLateral_Location = mkApp (mkCId "Lateral_Location") []
  gf GMicroscopic_Size = mkApp (mkCId "Microscopic_Size") []
  gf GNormal_Location = mkApp (mkCId "Normal_Location") []
  gf GNormal_Size = mkApp (mkCId "Normal_Size") []
  gf GSmall_Size = mkApp (mkCId "Small_Size") []
  gf GWidened_Size = mkApp (mkCId "Widened_Size") []
  gf Gmm_Size = mkApp (mkCId "mm_Size") []
  gf Gmm_lessThan_Size = mkApp (mkCId "mm_lessThan_Size") []

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "Abnormal_Location" -> GAbnormal_Location 
      Just (i,[]) | i == mkCId "Abnormal_Size" -> GAbnormal_Size 
      Just (i,[]) | i == mkCId "External_Location" -> GExternal_Location 
      Just (i,[]) | i == mkCId "Lateral_Location" -> GLateral_Location 
      Just (i,[]) | i == mkCId "Microscopic_Size" -> GMicroscopic_Size 
      Just (i,[]) | i == mkCId "Normal_Location" -> GNormal_Location 
      Just (i,[]) | i == mkCId "Normal_Size" -> GNormal_Size 
      Just (i,[]) | i == mkCId "Small_Size" -> GSmall_Size 
      Just (i,[]) | i == mkCId "Widened_Size" -> GWidened_Size 
      Just (i,[]) | i == mkCId "mm_Size" -> Gmm_Size 
      Just (i,[]) | i == mkCId "mm_lessThan_Size" -> Gmm_lessThan_Size 


      _ -> error ("no Describes " ++ show t)

instance Gf GDescription where
  gf (GAggregateDescriptor2Neg x1 x2 x3 x4 x5) = mkApp (mkCId "AggregateDescriptor2Neg") [gf x1, gf x2, gf x3, gf x4, gf x5]
  gf (GAggregateDescriptor2Pos x1 x2 x3 x4 x5) = mkApp (mkCId "AggregateDescriptor2Pos") [gf x1, gf x2, gf x3, gf x4, gf x5]
  gf (GAggregateProperty2Neg x1 x2 x3 x4 x5) = mkApp (mkCId "AggregateProperty2Neg") [gf x1, gf x2, gf x3, gf x4, gf x5]
  gf (GAggregateProperty2Pos x1 x2 x3 x4 x5) = mkApp (mkCId "AggregateProperty2Pos") [gf x1, gf x2, gf x3, gf x4, gf x5]
  gf (GDescribeNeg x1 x2 x3) = mkApp (mkCId "DescribeNeg") [gf x1, gf x2, gf x3]
  gf (GDescribePos x1 x2 x3) = mkApp (mkCId "DescribePos") [gf x1, gf x2, gf x3]

  fg t =
    case unApp t of
      Just (i,[x1,x2,x3,x4,x5]) | i == mkCId "AggregateDescriptor2Neg" -> GAggregateDescriptor2Neg (fg x1) (fg x2) (fg x3) (fg x4) (fg x5)
      Just (i,[x1,x2,x3,x4,x5]) | i == mkCId "AggregateDescriptor2Pos" -> GAggregateDescriptor2Pos (fg x1) (fg x2) (fg x3) (fg x4) (fg x5)
      Just (i,[x1,x2,x3,x4,x5]) | i == mkCId "AggregateProperty2Neg" -> GAggregateProperty2Neg (fg x1) (fg x2) (fg x3) (fg x4) (fg x5)
      Just (i,[x1,x2,x3,x4,x5]) | i == mkCId "AggregateProperty2Pos" -> GAggregateProperty2Pos (fg x1) (fg x2) (fg x3) (fg x4) (fg x5)
      Just (i,[x1,x2,x3]) | i == mkCId "DescribeNeg" -> GDescribeNeg (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2,x3]) | i == mkCId "DescribePos" -> GDescribePos (fg x1) (fg x2) (fg x3)


      _ -> error ("no Description " ++ show t)

instance Gf GDescriptions where
  gf (GDesc1 x1) = mkApp (mkCId "Desc1") [gf x1]
  gf (GDesc2 x1 x2) = mkApp (mkCId "Desc2") [gf x1, gf x2]
  gf (GDesc3 x1 x2 x3) = mkApp (mkCId "Desc3") [gf x1, gf x2, gf x3]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "Desc1" -> GDesc1 (fg x1)
      Just (i,[x1,x2]) | i == mkCId "Desc2" -> GDesc2 (fg x1) (fg x2)
      Just (i,[x1,x2,x3]) | i == mkCId "Desc3" -> GDesc3 (fg x1) (fg x2) (fg x3)


      _ -> error ("no Descriptions " ++ show t)

instance Gf GDescriptor where
  gf GAbnormal = mkApp (mkCId "Abnormal") []
  gf GExternal = mkApp (mkCId "External") []
  gf GLateral = mkApp (mkCId "Lateral") []
  gf GMicroscopic = mkApp (mkCId "Microscopic") []
  gf GNormal = mkApp (mkCId "Normal") []
  gf GSmall = mkApp (mkCId "Small") []
  gf GWidened = mkApp (mkCId "Widened") []
  gf (Gmm x1) = mkApp (mkCId "mm") [gf x1]
  gf (GmmLessThan x1) = mkApp (mkCId "mmLessThan") [gf x1]

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "Abnormal" -> GAbnormal 
      Just (i,[]) | i == mkCId "External" -> GExternal 
      Just (i,[]) | i == mkCId "Lateral" -> GLateral 
      Just (i,[]) | i == mkCId "Microscopic" -> GMicroscopic 
      Just (i,[]) | i == mkCId "Normal" -> GNormal 
      Just (i,[]) | i == mkCId "Small" -> GSmall 
      Just (i,[]) | i == mkCId "Widened" -> GWidened 
      Just (i,[x1]) | i == mkCId "mm" -> Gmm (fg x1)
      Just (i,[x1]) | i == mkCId "mmLessThan" -> GmmLessThan (fg x1)


      _ -> error ("no Descriptor " ++ show t)

instance Gf GDualOrgan where
  gf GKidney = mkApp (mkCId "Kidney") []
  gf GOvary = mkApp (mkCId "Ovary") []

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "Kidney" -> GKidney 
      Just (i,[]) | i == mkCId "Ovary" -> GOvary 


      _ -> error ("no DualOrgan " ++ show t)

instance Gf GNumber where
  gf GNum20 = mkApp (mkCId "Num20") []

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "Num20" -> GNum20 


      _ -> error ("no Number " ++ show t)

instance Gf GOrgan where
  gf (GBoth x1) = mkApp (mkCId "Both") [gf x1]
  gf GHeart = mkApp (mkCId "Heart") []
  gf (GLeft x1) = mkApp (mkCId "Left") [gf x1]
  gf (GRight x1) = mkApp (mkCId "Right") [gf x1]
  gf GSpleen = mkApp (mkCId "Spleen") []

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "Both" -> GBoth (fg x1)
      Just (i,[]) | i == mkCId "Heart" -> GHeart 
      Just (i,[x1]) | i == mkCId "Left" -> GLeft (fg x1)
      Just (i,[x1]) | i == mkCId "Right" -> GRight (fg x1)
      Just (i,[]) | i == mkCId "Spleen" -> GSpleen 


      _ -> error ("no Organ " ++ show t)

instance Gf GProperty where
  gf GLocation = mkApp (mkCId "Location") []
  gf GSize = mkApp (mkCId "Size") []

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "Location" -> GLocation 
      Just (i,[]) | i == mkCId "Size" -> GSize 


      _ -> error ("no Property " ++ show t)

instance Gf GStatement where
  gf (GPred x1 x2) = mkApp (mkCId "Pred") [gf x1, gf x2]
  gf (GPredAdv x1 x2) = mkApp (mkCId "PredAdv") [gf x1, gf x2]

  fg t =
    case unApp t of
      Just (i,[x1,x2]) | i == mkCId "Pred" -> GPred (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "PredAdv" -> GPredAdv (fg x1) (fg x2)


      _ -> error ("no Statement " ++ show t)


