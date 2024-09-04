data Measure unit = Measure Double unit
  deriving (Show)

double :: Measure unit -> Measure unit
double (Measure x unit) = Measure (2 * x) unit

-- Example usage
main :: IO ()
main = do
  let length = Measure 5.0 "meters"
  print length           -- Output: Measure 5.0 "meters"
  print (double length)  -- Output: Measure 10.0 "meters"