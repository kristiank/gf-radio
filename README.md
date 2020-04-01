# Radiology grammar in GF

Small grammar with expressions about radiology.

`Radiology.gf`, and concretes `Radiology{Eng,Est}.gf` contain expressions like the following.

> `Radiology> l Pred (Right Kidney) (Desc2 (DescribePos Abnormal Location Abnormal_Location) (DescribePos (mm Num20) Size mm_Size))`  
> right kidney's location is abnormal and size is 20 mm

`ExtRadiology.gf` does not contain new predicates, but instead new ways of presenting the information in `Radiology`. For instance:

> *right kidney's location is abnormal and size is 20 mm*  
> Variants and translations:  
 ↳ Eng: right kidney's location is abnormal and size is 20 mm  
 ↳ Eng: right kidney's location isn't normal and size is 20 mm  
 ↳ Eng: right kidney is with abnormal location and with 20 mm size  
 ↳ Eng: right kidney is without normal location and with 20 mm size  
 ↳ Est: parem neer on ebanormaalse asendiga ja 20 mmi suurusega  
 ↳ Est: parem neer on ilma normaalse asendita ja 20 mmi suurusega

All of these variants have a different AST. The mapping between them (*semantics-preserving transfer*) is done in Transfer.hs.

## Installation

1. In the main directory, compile GF grammars into PGF:

  ```
  gf -make grammars/Radiology*.gf grammars/ExtRadiology*.gf
  ```

1. Run `stack build`


## Use

1. Run `stack run transfer`
1. When prompted "Write your sentence here", write either a sentence without quotes (e.g. `heart's size is normal`), or `random`.
