concrete RadiologyEst of Radiology =
  open Prelude, SyntaxEst, ParadigmsEst,
  LexiconEst, (G=GrammarEst), --(E=ExtendEst),
  (R=ResEst) in {

  lincat
    Statement = S ;
    Description = {s : S ; adv : Adv} ;

    Organ,
    DualOrgan = OrganType ;

    Property = CN ;
    Descriptor = AP ;

  lin
    Pred org descn = mkS (topicAdv org) descn.s ; -- "the heart's size is normal"
    PredAdv org descn = mkS (mkCl (orgNP org) descn.adv) ;

    DescribePos descr prop proof = {
        s = mkS (lin Adv proof)
                (mkS (mkCl (mkNP prop) descr)) ;
        adv = SyntaxEst.mkAdv (casePrep comitative) (mkNP (mkCN descr prop)) } ;

    DescribeNeg descr prop proof = {
        s = mkS (lin Adv proof)
                (mkS negativePol (mkCl (mkNP prop) descr)) ;
        adv = SyntaxEst.mkAdv (prePrep abessive "ilma") (mkNP (mkCN descr prop))} ;

    Desc2 d1 d2 = d1 ** {
      s = G.ConjS and_Conj (G.BaseS d1.s d2.s) ;
      adv = G.ConjAdv and_Conj (G.BaseAdv d1.adv d2.adv)} ;

    Desc3 d1 d2 d3 = {
      s = G.ConjS and_Conj (G.ConsS d3.s
                                    (G.BaseS d1.s d2.s)
                            ) ;
      adv = G.ConjAdv and_Conj (G.ConsAdv d3.adv (G.BaseAdv d2.adv d1.adv))} ;

    AggregateProperty2Pos dr p q pr1 pr2 = {
      s = aggregateProperty dr p q pr1 pr2 positivePol ;
      adv = SyntaxEst.mkAdv (casePrep comitative) (mkNP (mkCN dr (prop2 p q)))
      } ;

    AggregateProperty2Neg dr p q pr1 pr2 = {
      s = aggregateProperty dr p q pr1 pr2 negativePol ;
      adv = SyntaxEst.mkAdv (prePrep abessive "ilma") (mkNP (mkCN dr (prop2 p q)))
      } ;

    Both org = org ** {s = mkCN (mkA (mkN "mõlema" "mõlema" "mõlemat")) org.s} ;
    Left org = {s = mkCN (mkA (mkN "vasak" "vasaku")) org.s ; pl = False} ;
    Right org = {s = mkCN (mkA (mkN "parem" "parema")) org.s ; pl = False} ;

    Heart = organ heart_N ;
    Spleen = organ (mkN "põrn" "põrna") ;

    Kidney = organ (mkN "neer" "neeru") ;
    Ovary = dualorgan (mkN "munasari" "munasarja" "munasarja" "munasarjasse" "munasarjade" "munasarju") ;

    Size = mkCN (mkN "suurus") ;
    Location = mkCN (mkN "asetus") ;

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

  prop2 : CN -> CN -> CN = \p1,p2 ->
    G.ConjCN and_Conj (G.BaseCN p1 p2) ;

  --mm_A : A = mkA "mm" "mm" "mm" "mm" ;
  mm_A : A = mkA "mm" ;

  OrganType : Type = {s : CN ; pl : Bool} ;

  organ : N -> OrganType = \n -> {
    s = mkCN n ; pl = False
    } ;
  dualorgan : N -> OrganType = \n ->
    organ n ** {pl = True} ;

  topicAdv : OrganType -> Adv = \org ->
    SyntaxEst.mkAdv (casePrep genitive) (mkNP org.s) ;

  orgNP : OrganType -> NP = \org ->
        case org.pl of {
          True => mkNP aPl_Det org.s ;
          False => mkNP org.s } ;

  aggregateProperty : Descriptor -> (p1, p2 : Property)
                      -> (pr1, pr2 : SS)
                      -> Pol
                      -> S ;
  aggregateProperty dr p q pr1 pr2 pol =
    mkS (lin Adv (cc2 pr1 pr2))
        (mkS pol (mkCl (plNP (prop2 p q)) dr)
        |mkS pol (mkCl (mkNP (prop2 p q)) dr)) ;

  plNP : CN -> NP = \cn ->
    let sgnp : NP = mkNP cn ;
        plnp : NP = mkNP aPl_Det cn ;
     in sgnp ** {a = plnp.a} ;

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
    mm_Size,
    mm_lessThan_Size = ss "" ;

}
