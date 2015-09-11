

hi :: b -> Maybe a
hi b = Just (butt b) where
  {- butt :: c -> d  -}
  butt = b ++ "String"
