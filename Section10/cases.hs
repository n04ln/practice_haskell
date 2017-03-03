data Prob a = Pr [(a, Rational) ]

uncastProb :: Prob a -> [(a, Rational) ]
uncastProb (Pr ps) = ps
