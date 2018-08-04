module SceneGraph where


data Tree a = Empty | Node a [Tree a]

makeNode :: a -> Tree a
makeNode a = Node a []

instance Functor Tree where
  fmap f Empty = Empty
  fmap f (Node a xs) = Node (f a) (fmap (fmap f) xs)

apply :: (a -> IO ()) -> Tree a  -> IO ()
apply _ Empty  = return () 
apply f (Node a xs) = do
  _ <- f a
  mapM_ (apply f) xs

data SceneGraph a = SceneGraph (Tree a)
instance Functor SceneGraph where
  fmap f (SceneGraph tree) = SceneGraph (fmap f tree)
  

makeSceneGraph :: a -> SceneGraph a
makeSceneGraph a = SceneGraph (makeNode a)