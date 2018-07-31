module Matrix where


data GLMatrix a =
  GLMatrix !a !a !a !a
           !a !a !a !a
           !a !a !a !a
           !a !a !a !a
             deriving Eq

