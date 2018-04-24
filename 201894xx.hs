--data Bool = False | True

data Week = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday


instance Eq Week where
  Monday == Monday  = True
  Tuesday  == Tuesday  = True
  Wednesday  == Wednesday = True
  Thursday  == Thursday  = True
  Friday == Friday  = True
  Saturday  == Saturday  = True
  Sunday == Sunday  = True
  _  == _  = False

-- Below, the commented-out lines are for defining each case.  If
-- measurement of equality works, the seven cases of a DoW matching
-- itself can be compressed.
-- Once all EQ and LT cases are handled, the remainder
-- as in "compare _ _" are GT.
instance Ord Week where
  compare d1 d2
    | d1 == d2 = EQ
--  compare Monday Monday = EQ
  compare Monday Tuesday = LT
  compare Monday Wednesday = LT
  compare Monday Thursday = LT
  compare Monday Friday = LT
  compare Monday Saturday = LT
  compare Monday Sunday = LT
--  compare Tuesday Monday = GT
--  compare Tuesday Tuesday = EQ
  compare Tuesday Wednesday = LT
  compare Tuesday Thursday = LT
  compare Tuesday Friday = LT
  compare Tuesday Saturday = LT
  compare Tuesday Sunday = LT
--  compare Wednesday Monday = GT
--  compare Wednesday Tuesday = GT
--  compare Wednesday Wednesday = EQ
  compare Wednesday Thursday = LT
  compare Wednesday Friday = LT
  compare Wednesday Saturday = LT
  compare Wednesday Sunday = LT
--  compare Thursday Monday = GT
--  compare Thursday Tuesday = GT
--  compare Thursday Wednesday = GT
--  compare Thursday Thursday = EQ
  compare Thursday Friday = LT
  compare Thursday Saturday = LT
  compare Thursday Sunday = LT
--  compare Friday Monday = GT
--  compare Friday Tuesday = GT
--  compare Friday Wednesday = GT
--  compare Friday Thursday = GT
--  compare Friday Friday = EQ
  compare Friday Saturday = LT
  compare Friday Sunday = LT
--  compare Saturday Monday = GT
-- compare Saturday Tuesday = GT
--  compare Saturday Wednesday = GT
--  compare Saturday Thursday = GT
--  compare Saturday Friday = GT
--  compare Saturday Saturday = EQ
  compare Saturday Sunday = LT
--  compare Sunday Monday = GT
--  compare Sunday Tuesday = GT
--  compare Sunday Wednesday = GT
--  compare Sunday Thursday = GT
--  compare Sunday Friday = GT
--  compare Sunday Saturday = GT
--  compare Sunday Sunday = EQ
  compare _ _ = GT

instance Show Week where
  show Monday = "Monday"
  show Tuesday = "Tuesday"
  show Wednesday = "Wednesday"
  show Thursday = "Thursday"
  show Friday = "Friday"
  show Saturday = "Saturday"
  show Sunday = "Sunday"


instance Enum Week where
--  toEnum :: Int -> Week
  toEnum 1 =  Monday
  toEnum 2 =  Tuesday
  toEnum 3 = Wednesday
  toEnum 4 =  Thursday
  toEnum 5 =  Friday
  toEnum 6 =  Saturday
  toEnum 7 =  Sunday

--  fromEnum :: Week -> Int
  fromEnum Monday    = 1
  fromEnum Tuesday   = 2
  fromEnum Wednesday = 3 
  fromEnum Thursday  = 4
  fromEnum Friday    = 5
  fromEnum Saturday  = 6
  fromEnum Sunday    = 7


instance Bounded Week where
  minBound = Monday
  maxBound = Sunday
