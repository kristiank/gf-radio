concrete ExtRadiologyEst of ExtRadiology = RadiologyEst **
  open Prelude, SyntaxEst, ParadigmsEst, (G=GrammarEst) in {

lin
    PredAdv org descn = mkS (mkCl (orgNP org) descn.adv) ;

    AggregateProperty2Pos dr p q pr1 pr2 = {
      s = aggregateProperty dr p q pr1 pr2 positivePol ;
      adv = SyntaxEst.mkAdv (casePrep comitative) (mkNP (mkCN dr (prop2 p q)))
      } ;

    AggregateProperty2Neg dr p q pr1 pr2 = {
      s = aggregateProperty dr p q pr1 pr2 negativePol ;
      adv = SyntaxEst.mkAdv (prePrep abessive "ilma") (mkNP (mkCN dr (prop2 p q)))
      } ;

    AggregateDescriptor2Pos pr d e pr1 pr2 = {
      s = aggregateDescriptor pr d e pr1 pr2 positivePol ;
      adv = SyntaxEst.mkAdv (casePrep comitative) (mkNP (mkCN (desc2 d e) pr))
      } ;

    AggregateDescriptor2Neg pr d e pr1 pr2 = {
      s = aggregateDescriptor pr d e pr1 pr2 negativePol ;
      adv = SyntaxEst.mkAdv without_Prep (mkNP (mkCN (desc2 d e) pr))
      } ;

oper
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

  aggregateDescriptor : Property -> (d1, d2 : Descriptor)
                      -> (pr1, pr2 : SS)
                      -> Pol
                      -> S ;
  aggregateDescriptor pr d e pr1 pr2 pol =
    mkS (lin Adv (cc2 pr1 pr2))
        (mkS pol (mkCl (mkNP pr) (desc2 d e))) ;

  desc2 : AP -> AP -> AP =  \d1,d2 ->
    G.ConjAP and_Conj (G.BaseAP d1 d2) ;

}
