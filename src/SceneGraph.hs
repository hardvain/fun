module SceneGraph where

data Tree a = Empty | Node a [Tree a]

instance Functor Tree where
  fmap f Empty = Empty
  fmap f (Node a xs) = Node (f a) (fmap (fmap f) xs)

data SceneGraph a = SceneGraph (Tree a)

instance Functor SceneGraph where
  fmap f (SceneGraph tree) = SceneGraph (fmap f tree)

