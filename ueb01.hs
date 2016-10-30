rgb2cmyk :: Int -> Int -> Int -> (Float, Float, Float, Float)
rgb2cmyk 0 0 0 = (0, 0, 0, 1)
rgb2cmyk r g b =
    let c = (w - (fromIntegral(r) / 255)) / w
        m = (w - (fromIntegral(g) / 255)) / w
        y = (w - (fromIntegral(b) / 255)) / w
        k = 1 - w
    in (c, m, y, k)
    where w = fromIntegral(maximum [r, g, b]) / 255

pythagoras_tripel :: Int -> Int -> Int -> Bool
pythagoras_tripel a b c =
    let hypo = maximum [a, b, c]
        cat1 = minimum [a, b, c]
        cat2 = median a b c
    in hypo ^ 2 == cat1 ^ 2 + cat2 ^ 2

median :: Int -> Int -> Int -> Int
median a b c =
    let x = maximum [a, b, c]
    in if a == x then max b c
    else if b == x then max a c
    else max a b
