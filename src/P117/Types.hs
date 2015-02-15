
module P117.Types where

data TreeItem = TreeItem { tiTitle :: String
                         , tiPageId :: Integer
                         }

data PredicateType = CustomPredicate | AllPagesPredicate
