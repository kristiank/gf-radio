abstract Radiology = {

flags startcat = Statement ;

  cat
    Statement ;
    Description ;
    Descriptions ;

    Organ ;
    DualOrgan ;

    Property ;

    Descriptor ;
    Describes Descriptor Property ; -- proof objects

    Number ;

  data
    Pred : Organ -> Descriptions -> Statement ;

    DescribePos,
    DescribeNeg : (d : Descriptor) -> (p : Property) -> Describes d p -> Description ;

    Desc1 : Description -> Descriptions ;
    Desc2 : Description -> Description -> Descriptions ;
    Desc3 : Description -> Description -> Description -> Descriptions ;

    Both,
    Left,
    Right : DualOrgan -> Organ ;

    Heart,
    Spleen : Organ ;

    Ovary,
    Kidney : DualOrgan ;

    Size,
    Location : Property ;

    Lateral,
    External : Descriptor ; -- Location descriptors

    Small,
    Widened,
    Microscopic : Descriptor ; -- Size descriptors
    mm,                              -- 20 mm
    mmLessThan : Number -> Descriptor ; -- <20mm

    Num20 : Number ;

    Normal, Abnormal : Descriptor ; -- Can be any descriptor
    -- Parallel, Horizontal : Descriptor  -- Orientation descriptor

    -- Proof objects
    Small_Size : Describes Small Size ;
    Widened_Size : Describes Widened Size ;
    Microscopic_Size : Describes Microscopic Size ;
    Normal_Size : Describes Normal Size ;
    Abnormal_Size : Describes Abnormal Size ;
    Lateral_Location : Describes Lateral Location ;
    External_Location : Describes External Location ;
    Normal_Location : Describes Normal Location ;
    Abnormal_Location : Describes Abnormal Location ;
    mm_Size : Describes (mm Num20) Size ;
    mm_lessThan_Size : Describes (mmLessThan Num20) Size ;
}
