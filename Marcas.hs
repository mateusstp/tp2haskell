
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


{--url para consulta  de marcas--}
jsonURLMarca :: String
jsonURLMarca = "http://fipeapi.appspot.com/api/1/carros/marcas.json"
	
{--consulta url e retorna marcas--}
getJSONMarca :: IO BL.ByteString
getJSONMarca = simpleHttp jsonURLMarca

{--retorna um lista de tuplas [(id de marcas, nome marca)]--}
listaIdMarcas :: [Marca] -> [(Int,String)]
listaIdMarcas m = [(idM x, fipe_nameM x) | x <- m]

{--retorna um lista de tuplas [(id de marcas, nome marca)]--}
{--caso a consulta retorne erro retorna uma lista vazia--}
marcas ::IO [(Int,String)]
marcas = do	
	d <- (eitherDecode <$> getJSONMarca) :: IO (Either String [Marca])
	case d of
		Left err -> return [] 
		Right ps -> return (listaIdMarcas ps)
			