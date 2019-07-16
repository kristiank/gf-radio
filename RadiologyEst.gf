concrete RadiologyEst of Radiology =
  open Prelude, SyntaxEst, ParadigmsEst,
  LexiconEst, (G=GrammarEst), --(E=ExtendEst),
  (R=ResEst) in {

  lincat
    Statement = S ;
    Description = {s : R.Polarity => S} ;
    Descriptions2,
    Descriptions3 = {s : R.Polarity => [S]} ;

    Organ,
    DualOrgan = OrganType ;

    Property = NP ;
    Descriptor = AP ;

  lin
    Pred org descn = mkS (topicAdv org) (descn.s ! positivePol.p) ; -- "the heart's size is normal"
    PredNeg org descn = mkS (topicAdv org) (descn.s ! negativePol.p) ;
    Describe descr prop _ = {s = \\p => mkS (pol p) (mkCl prop descr)} ;

    Desc2 d1 d2 = {s = \\p => G.BaseS (d1.s ! p) (d2.s ! p)} ;
--    Desc3 d1 d2 d3 = {s = \\p => G.ConsS (d3.s ! p) (Desc2 d2 d1)} ;
    Desc2as3 d = d ;
    ConjDesc2 descs = {s = \\p => G.ConjS and_Conj (descs.s ! p)} ;
--    ConjDesc3 =

    AggregateProperty2 dr p q _ _ = {s = \\pl => mkS (pol pl) (mkCl (prop2 p q) dr)} ;

    Both org = org ** {s = mkCN (mkA (mkN "mõlema" "mõlema" "mõlemat")) org.s ; pl = False} ;
    Left org = {s = mkCN (mkA (mkN "vasak" "vasaku")) org.s ; pl = False} ;
    Right org = {s = mkCN (mkA (mkN "parem" "parema")) org.s ; pl = False} ;

    Heart = organ heart_N ;
    Spleen = organ (mkN "põrn" "põrna") ;

    Kidney = dualorgan (mkN "neer" "neeru") ;
    Ovary = dualorgan (mkN "munasari" "munasarja" "munasarja" "munasarjasse" "munasarjade" "munasarju") ;

    Size = mkNP (mkN "suurus") ;
    Location = mkNP (mkN "asetus") ;

    Lateral = mkAP (mkA "lateraalne") ;
    External = mkAP (mkA "väline") ;

    Small = mkAP small_A ;
    Normal = mkAP (mkA "normaalne") ;
    Abnormal = mkAP (mkA "ebanormaalne") ;
    Widened = mkAP (mkA "laienenud") ;
    Microscopic = mkAP (mkA "mikroskoopiline") ;

    mm int = mkAP (lin AdA int) mm_A ;
    mmLessThan int = mkAP (lin AdA (cc2 {s="<"} int)) mm_A ;

oper

  prop2 : NP -> NP -> NP = \p1,p2 ->
    G.ConjNP and_Conj (G.BaseNP p1 p2) ;

  --mm_A : A = mkA "mm" "mm" "mm" "mm" ;
  mm_A : A = mkA "mm" ;

  OrganType : Type = {s : CN ; pl : Bool} ;

  organ : N -> OrganType = \n -> {
    s = mkCN n ; pl = False
    } ;
  dualorgan : N -> OrganType = \n ->
    organ n ** {pl = True} ;

  topicAdv : OrganType -> Adv = \org ->
    SyntaxEst.mkAdv (casePrep genitive) (orgNP org)
    where {
     orgNP : OrganType -> NP = \org ->
        case org.pl of {
          True => mkNP aPl_Det org.s ;
          False => mkNP org.s }
      }
    ;

  pol : R.Polarity -> Pol = \p -> case p of {
    R.Pos => positivePol ;
    R.Neg => negativePol
  } ;
lin

-- proof objects
    Small_Size,
    Widened_Size,
    Microscopic_Size,
    Normal_Size,
    Abnormal_Size,
    Lateral_Location,
    External_Location,
    Normal_Location,
    Abnormal_Location,
    -- Normal_Size_Location,
    -- Abnormal_Size_Location,
    mm_Size,
    mm_lessThan_Size = ss "proof object" ;

}
