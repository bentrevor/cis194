module Employee where

data Tree a = Node { rootLabel :: a
                   , subForest :: [Tree a]
                   }

-- Employee names are represented by Strings.
type Name = String

-- The amount of fun an employee would have at the party, represented
-- by an Integer
type Fun  = Integer

-- An Employee consists of a name and a fun score.
data Employee = Emp { empName :: Name, empFun :: Fun }
  deriving (Show, Read, Eq)

-- A type to store a list of guests and their total fun score.
data GuestList = GL [Employee] Fun
  deriving (Show, Eq)

instance Ord GuestList where
  compare (GL _ f1) (GL _ f2) = compare f1 f2
