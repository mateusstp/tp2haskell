
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

{--gera consulda para id de car informada marca informada--}
getJSONVeiculos :: Int -> IO B.ByteString
getJSONVeiculos idMarca = simpleHttp $"http://fipeapi.appspot.com/api/1/carros/veiculos/"++ show idMarca ++ ".json"

{--gera lista de veiculos para marca informada, caso a 
consulta falhe retorna uma lista vazia--}
geraListaVeiculos :: Int -> IO [Veiculos]
geraListaVeiculos idm = do
	--menagem para gera log de consultas 
	let msg = ("aguarde... Consulta de veiculos em andamento... Marca: "++(show idm)++"\n")
	print  msg
	appendFile "log.txt" msg 
	resultado <- (eitherDecode <$> (getJSONVeiculos idm) ) :: IO (Either String [Veiculos])
	case resultado of
		Left err -> return []
		Right ps -> return ps

{--retorna lista de [[veiculos]] --}
geraConsultaVeiculos :: IO [[Veiculos]]
geraConsultaVeiculos = do
	{--recebe lista de tupla [(ID,MARCA)]--}
	listaTuplaIdMarcas  <- marcas
	{--gera lista [ID] das marcas--}
	let listaIDMarcas = L.map fst listaTuplaIdMarcas
	{--retorna lista de [veiculos] cada marca lofo [[veiculos]] 
	veiculos podem ser compreendidos como modelos existentes de cada marca--}
	listaVeiculos     <- sequence( L.map geraListaVeiculos listaIDMarcas)
	return (listaVeiculos)
