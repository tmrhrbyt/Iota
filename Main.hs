import Text.Parsec
import Text.Parsec.String (Parser)
import qualified Text.Parsec as P
import Control.Applicative ((<$>), (<*>))
import Control.Monad (void)

data Iota = I
          | O Iota Iota
    deriving Eq

instance Show Iota where
    show (I)     = "ι"
    show (O x y) = "(" ++ show x ++ show y ++ ")"

exp :: Parser Iota
exp = parseI P.<|> parseO
    where parseI = I <$ P.char '1'
          mkO [x,y] = O x y
          parseO = mkO <$> (P.char '0' >> P.count 2 exp)

parseIota :: String -> Either P.ParseError Iota
parseIota = P.parse exp "woop woop"
itb :: Iota -> String
itb I       = "1"
itb (O l r) = "0" ++ itb l ++ itb r
s, k, i :: Iota
s = O I k
k = O I $ O I $ O I I
i = O (O s k) k

m :: Iota
m = O (O s i) i

y :: Iota
y = O (O skm ssksk) km
    where skm   = O s km
          ssksk = O s (O (O s (O k s)) k)
          km    = O k m
b, c, w :: Iota
b = O (O s (O k s)) k
c = O (O s (O (O s (O k b)) s)) (O k k)
w = O (O s s) (O s k)
t, f :: Iota
t = k
f = O s k
omega :: Iota
omega = O m m
