
{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module Precos where
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
import Carros

--{"fipe_marca": "Fiat", "referencia": "Fevereiro de 2013", "fipe_codigo": "001267-0", "preco": "R$ 21.712,00", "name": "2011 Gasolina", "key": "2011-1980274", "veiculo": "Palio 1.0 ECONOMY Fire Flex 8V 4p", "id": "1980274", "marca": "FIAT"}
 --http://fipeapi.appspot.com/api/1/carros/veiculo/21/001267-0/1980274.json

data Precos =
	 Precos {  fipe_marcaP  :: String
			  ,referenciaP  :: String
			  ,fipe_codigoP :: String
			  ,precoP       :: String
			  ,nameP        :: String
			  ,keyP         :: String 
			  ,veiculoP     :: String
			  ,idP          :: String
			  ,marcaP       :: String
			 }deriving Show
	
instance FromJSON Precos where 
	parseJSON (Object p) =
		  Precos <$> p .: "fipe_marca" 
				 <*> p .: "referencia" 
				 <*> p .: "fipe_codigo"
				 <*> p .: "preco"      
				 <*> p .: "name"       
				 <*> p .: "key"        
				 <*> p .: "veiculo"    
				 <*> p .: "id"         
				 <*> p .: "marca"       
	parseJSON _= mzero 

getJSONPrecos :: Int -> String -> IO B.ByteString
getJSONPrecos idMarca idVeiculo = simpleHttp $ "http://fipeapi.appspot.com/api/1/carros/veiculo/"++(show idMarca)++"/"++idVeiculo ++".json"

geraListaPrecos :: (String ,Int) -> IO [Precos]
geraListaPrecos tuplaIdmIdC = do 
	resultado <- (eitherDecode <$> (getJSONPrecos (snd tuplaIdmIdC) (fst tuplaIdmIdC)) :: IO (Either String [Precos]))
	case resultado of
		Left err -> return []
		Right ps -> return ps

geraListaIdc :: [Carros] -> [String]
geraListaIdc c = L.map idC c

aplicaMarcaP :: [Int] -> [String] -> [(String, Int)]
aplicaMarcaP ids ls =  L.foldr (++) [] (L.map (L.zip ls) (L.map (L.replicate (L.length ls)) ids))

geraConsultaPrecos = do
	{--gera tupla(ID,MARCA)--}
	lMarcas  <- marcas
	{--gera lista [ID] das marcas--}
	let lIdMarcas = L.map fst lMarcas
	--{--gera lista carros[[[Carros]]] --}
	--carros <- geraConsultaCarros
	let lIdc = L.foldr (++) [] (L.map (L.foldr (++) []) (L.map (L.map geraListaIdc ) carros))
	let tuplaConsulta = aplicaMarcaP lIdMarcas lIdc
	listaPrecos     <- sequence( L.map geraListaPrecos tuplaConsulta)
	--return (listaVeiculos)
	return (listaPrecos)


