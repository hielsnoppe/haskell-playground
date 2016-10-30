rgb2cmyk :: Int -> Int -> Int -> (Float, Float, Float, Float)
rgb2cmyk 0 0 0 = (0, 0, 0, 1)
rgb2cmyk r g b =
    let c = (w - (fromIntegral(r) / 255)) / w
        m = (w - (fromIntegral(g) / 255)) / w
        y = (w - (fromIntegral(b) / 255)) / w
        k = 1 - w
    in (c, m, y, k)
    where w = fromIntegral(maximum [r, g, b]) / 255
