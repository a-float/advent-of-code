maxWithIndex :: (Ord a) => [(Int, a)] -> Maybe (Int, a)
maxWithIndex xs = foldr step Nothing xs
  where
    step x acc
      | acc == Nothing = Just x
      | Just (i, v) <- acc, v < snd x = Just x
      | otherwise = acc

part1 :: [Char] -> Int
part1 xs =
  let xsi = zip [1 ..] $ reverse xs
      Just (i, a) = maxWithIndex $ drop 1 xsi
      Just (_, b) = maxWithIndex $ take (i - 1) xsi
      _ = error "Failed"
   in read $ [a, b]

-- should put it in a loop/scanr
part2 :: [Char] -> Int
part2 xs =
  let xsi = zip [1 ..] $ reverse xs
      Just (i1, x1) = maxWithIndex $ drop 11 xsi
      Just (i2, x2) = maxWithIndex $ drop 10 $ take (i1 - 1) xsi
      Just (i3, x3) = maxWithIndex $ drop 9 $ take (i2 - 1) xsi
      Just (i4, x4) = maxWithIndex $ drop 8 $ take (i3 - 1) xsi
      Just (i5, x5) = maxWithIndex $ drop 7 $ take (i4 - 1) xsi
      Just (i6, x6) = maxWithIndex $ drop 6 $ take (i5 - 1) xsi
      Just (i7, x7) = maxWithIndex $ drop 5 $ take (i6 - 1) xsi
      Just (i8, x8) = maxWithIndex $ drop 4 $ take (i7 - 1) xsi
      Just (i9, x9) = maxWithIndex $ drop 3 $ take (i8 - 1) xsi
      Just (i10, x10) = maxWithIndex $ drop 2 $ take (i9 - 1) xsi
      Just (i11, x11) = maxWithIndex $ drop 1 $ take (i10 - 1) xsi
      Just (_, x12) = maxWithIndex $ drop 0 $ take (i11 - 1) xsi
   in read $ [x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12]

main :: IO ()
main = do
  ls <- lines <$> readFile "./data/day03.txt"

  print $ sum $ map part1 ls
  print $ sum $ map part2 ls