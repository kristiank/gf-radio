abstract ExtRadiology = Radiology ** {

flags startcat=Statement ;
-- Extensions for information aggregation and rephrasing
fun
    PredAdv : Organ -> Descriptions -> Statement ;

    -- To make text more natural; transfer rules in Haskell
    AggregateProperty2Pos,
    AggregateProperty2Neg : (d : Descriptor) -> (p1, p2 : Property)
                          -> Describes d p1
                          -> Describes d p2
                          -> Description ; -- size and location are normal / normaalse suuruse ja asetusega

    AggregateDescriptor2Pos,
    AggregateDescriptor2Neg : (p : Property) -> (d1, d2 : Descriptor)
                           -> Describes d1 p
                           -> Describes d2 p
                           -> Description ; -- location is normal and lateral

}
