
{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module Marcas where
import Data.Aeson
import Data.Text 
import Control.Applicative
import Control.Monad
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as Bs
import Network.HTTP.Conduit (simpleHttp)
import GHC.Generics
import qualified Data.ByteString.Char8 as C

{-data Marca =
     Marca { keyM       :: !Text
            ,idM        :: Int
            ,fipe_nameM :: !Text
            ,nameM      :: !Text
           } deriving Show-}

data Marca =
     Marca { keyM       :: String
            ,idM        :: Int
            ,fipe_nameM :: String
            ,nameM      :: String
           } deriving Show

instance FromJSON Marca where 
	parseJSON (Object m) =
		Marca <$> m .: "key"
			  <*> m .: "id"
			  <*> m .: "fipe_name"
			  <*> m .: "name"
	parseJSON _= mzero		  	


jsonURLMarca :: String
jsonURLMarca = "http://fipeapi.appspot.com/api/1/carros/marcas.json"
	
getJSONMarca :: IO BL.ByteString
getJSONMarca = simpleHttp jsonURLMarca

---listaIdMarcas :: [Marca] -> [(Int,Text)]
--listaIdMarcas m = [(getId x, getName x) | x <- m]

listaIdMarcas :: [Marca] -> [(Int,String)]
listaIdMarcas m = [(idM x, nameM x) | x <- m]

--getKey -:: Marca -> Text
--getKey (Marca getKey _ _ _ ) = getKey

--getFipe_name :: Marca -> Text
--getFipe_name (Marca _ _ getFipe_name _ ) = getFipe_name

--getName :: Marca -> Text
--getName (Marca _ _ _ getName ) = getName

--getId :: Marca -> Int
--getId (Marca _ getId _ _ ) = getId

marcas ::IO [(Int,String)]
marcas = do	
	d <- (eitherDecode <$> getJSONMarca) :: IO (Either String [Marca])
	case d of
		Left err -> return [] 
		Right ps -> return (listaIdMarcas ps)
			