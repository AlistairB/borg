data Email = Email
  { firstPart :: String
  , sld :: String
  }

email = "alistair@email.com"

emailParser :: Parser Email
emailParser = do
  firstPart <- some alphaNumChar
  char '@'
  sld <- some alphaNumChar
  string' ".com"
  pure $ Email firstPart sld
