
{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module Carros where
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
import Veiculos

{--fipe_marca,fipe_codigo,name,marca,key,veiculo,id--}
data Carros =
	Carros {  fipe_marcaC  :: String
			,fipe_codigoC :: String
			,nameC        :: String
			,keyC         :: String 
			,veiculoC     :: String
			,idC          :: String
			  }deriving Show
	
instance FromJSON Carros where 
	parseJSON (Object c) =
		Carros <$> c .: "fipe_marca" 
		<*> c .: "fipe_codigo"
		<*> c .: "name"
		<*> c .: "key"    
		<*> c .: "veiculo"
		<*> c .: "id" 
	parseJSON _= mzero 

getJSONCarros :: Int -> String -> IO B.ByteString
getJSONCarros idMarca idVeiculo = simpleHttp $ "http://fipeapi.appspot.com/api/1/carros/veiculo/"++(show idMarca)++"/"++idVeiculo++".json"

aplicaMarca :: [Int] -> [[String]] -> [[(Int, String)]]
aplicaMarca [] _ = []
aplicaMarca _ [] = []
aplicaMarca (id:ids) (l:ls) = L.zip (L.replicate (L.length l) id) l : aplicaMarca ids ls 

geraListaCarros :: (Int, String) -> IO [Carros]
geraListaCarros tuplaIdmIdv = do 
	resultado <- (eitherDecode <$> (getJSONCarros (fst tuplaIdmIdv) (snd tuplaIdmIdv) ) :: IO (Either String [Carros]))
	case resultado of
		Left err -> return []
		Right ps -> return ps

geraConsultaCarros = do
	{-- lista de [(IdMarca,NomeMarca)]--}
	marca <- marcas
	{-- lista de [IdMarca]--}
	let idm = L.map fst marca
	{-- lista de [[Veiculos]]--}
	lVeiculos <- geraConsultaVeiculos
	{-- lista de [[idv]]--}
	let listaIdv = L.map (L.map idV) lVeiculos
	{-- lista de [[(idm,idv)]]--}
	let tupla = aplicaMarca idm listaIdv
	carros <- sequence (L.map (mapM geraListaCarros) tupla)
	return 	carros
