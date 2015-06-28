
{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module Veiculos where
import Data.Aeson
import Data.Text 
import Control.Applicative
import Control.Monad
import qualified Data.ByteString.Lazy as B
import Network.HTTP.Conduit (simpleHttp)
import GHC.Generics
import qualified Data.List as L
import Data.Function
import Marcas
--{"fipe_marca": "Fiat", "referencia": "Fevereiro de 2013", "fipe_codigo": "001267-0", "preco": "R$ 21.712,00", "name": "2011 Gasolina", "key": "2011-1980274", "veiculo": "Palio 1.0 ECONOMY Fire Flex 8V 4p", "id": "1980274", "marca": "FIAT"}

data Veiculos =
	Veiculos { fipe_marcaV :: String
			  , fipe_nameV :: String
			  , marcaV :: String
			  , keyV :: String
			  , idV :: String
			  , nameV :: String
			}deriving Show

instance FromJSON Veiculos where 
	parseJSON (Object v) =
		Veiculos <$> v .: "fipe_marca"
			  	 <*> v .: "fipe_name"
			  	 <*> v .: "marca"
			  	 <*> v .: "key"
			  	 <*> v .: "id"
			  	 <*> v .: "name"
	parseJSON _= mzero 


{--gera consulda para marca informada--}
getJSONVeiculos :: Int -> IO B.ByteString
getJSONVeiculos idMarca = simpleHttp $"http://fipeapi.appspot.com/api/1/carros/veiculos/"++ show idMarca ++ ".json"

{--gera lista de veiculos para marca informada--}
geraListaVeiculos idm = do
	let msg = ("aguarde... Consulta de veiculos em andamento... Marca: "++(show idm)++"\n")
	print  msg
	appendFile "log.txt" msg 
	resultado <- (eitherDecode <$> (getJSONVeiculos idm) ) :: IO (Either String [Veiculos])
	case resultado of
		Left err -> return []
		Right ps -> return ps

{--gera lista de veiculos para marca informada--}
geraConsultaVeiculos = do
	{--gera tupla(ID,MARCA)--}
	listaTuplaIdMarcas  <- marcas
	{--gera lista [ID] das marcas--}
	let listaIDMarcas = L.map fst listaTuplaIdMarcas
	listaVeiculos     <- sequence( L.map geraListaVeiculos listaIDMarcas)
	return (listaVeiculos)
