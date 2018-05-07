(in-package :mma)

(defvar built-in-syms
  ;; these are the atoms used by the parser, evaluator, display,
  ;; etc.  They must be the same in each of the separate packages,
  ;; and so each package should be in this package ( :mma).
	  
    '(|AddTo| |Alias|
|Alternatives| ;; added 11/17/94
|And| |Apply| |Blank| |BlankNullSequence| |BlankSequence| 
|CompoundExpression| |Condition| |Delayed| |Derivative| |DivideBy| |Dot|
 |Equal| |Exit| 
|Factorial| |Factorial2| |False| |Function| |Greater| |GreaterEqual| |If| |In| |Increment|
|Inequality| |Integer| |Less| |LessEqual| |List| |Map| |MapAll| |MessageName|
      |NonCommutativeMultiply| |Not| |Null| |Optional| |Or| |Out| |Part| |Pattern|
|PatternTest| |Pi| |Plus| |Power| |PreDecrement| |PreIncrement| ;|PrimeQ| 
              |Put| |PutAppend|
      |Real| |Repeated| |RepeatedNull| |Replace| |ReplaceAll| |ReplaceRepeated|

      
|Rule| |RuleDelayed| |SameQ| |Sequence| |Set| |SetDelayed| |Slot| |SlotSequence|
      |SubtractFrom| |TagSet| |TagSetDelayed| |Times| |TimesBy| |True| |UnAlias| |Unequal|
     
|UnSameQ| |UnSet| |UpSet| |UpSetDelayed| |$Line| |Quote|)  ;; we added Quote.
  )
