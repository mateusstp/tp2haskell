
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

{--recebe uma id de marca  e um id de um veiculo/mdelo retorna uma 
lista de carros para cada marca, logo, uma [[Carros]], cada lista interna 
representa os carros de uma  marca --}
getJSONCarros :: Int -> String -> IO B.ByteString
getJSONCarros idMarca idVeiculo = simpleHttp $ "http://fipeapi.appspot.com/api/1/carros/veiculo/"++(show idMarca)++"/"++idVeiculo++".json"


{-- recebe um lista de id de marcas e uma lista de lista de id de veiculos/modelos
e gera uma  [[(Id Marca, Id do veiculo)]] --}
aplicaMarca :: [Int] -> [[String]] -> [[(Int, String)]]
aplicaMarca [] _ = []
aplicaMarca _ [] = []
aplicaMarca (id:ids) (l:ls) = L.zip (L.replicate (L.length l) id) l : aplicaMarca ids ls 

{--gera uma consulta de cada tupla (id marca, id veiculo/modelo) e 
retorna uma lista de carro [Carros], caso a consulta falhe retorna um lista vazia []--}
geraListaCarros :: (Int, String) -> IO [Carros]
geraListaCarros tuplaIdmIdv = do
	let msg = ("aguarde... Consulta de carros em andamento... Marca: "++(show (fst tuplaIdmIdv))++" Veiculos : " ++ (show (snd tuplaIdmIdv))++"\n")
	print  msg
	appendFile "log.txt" msg
	resultado <- (eitherDecode <$> (getJSONCarros (fst tuplaIdmIdv) (snd tuplaIdmIdv) ) :: IO (Either String [Carros]))
	case resultado of
		Left err -> return []
		Right ps -> return ps

{--Retorna uma [[[Carros]]], isto e, cada [Carros] representa os carros de uma marca
logo, [[Carros]] para todas as marcas consultas o terceiro encapsulamento de lista e
devido a funcao sequence :: Monad m => [m a] -> m [a]--}
geraConsultaCarros :: IO [[[Carros]]]
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
	{--recebe uma [[[Carros]]]--}
	carros <- sequence (L.map (mapM geraListaCarros) tupla)
	return 	carros
