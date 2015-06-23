
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


data Carros =
	 Carros {  fipe_marcaC  :: String
	          ,referenciaC  :: String
	          ,fipe_codigoC :: String
	          ,precoC       :: String
	          ,nameC       :: String
	          ,keyC         :: String 
	          ,veiculoC     :: String
	          ,idC          :: String
	          ,marcaC       :: String
	  		 }deriving Show
	
instance FromJSON Carros where 
	parseJSON (Object v) =
		  Carros <$> v .: "fipe_marca" 
          		 <*> v .: "referencia" 
          		 <*> v .: "fipe_codigo"
          		 <*> v .: "preco"      
          		 <*> v .: "name"       
          		 <*> v .: "key"        
          		 <*> v .: "veiculo"    
          		 <*> v .: "id"         
          		 <*> v .: "marca"      	
	parseJSON _= mzero 


getJSONCarro :: String -> String -> String -> IO B.ByteString
getJSONCarro idMarca idCarro tipoCombustivel  = simpleHttp $http://"fipeapi.appspot.com/api/1/carros/veiculo/"++ show idMarca ++"/"++ idCarro ++"/"++tipoCombustivel++".json"

geraListaCarros idm idc idt = do 
	resultado <- (eitherDecode <$> (getJSONCarros idm idc idt) ) :: IO (Either String [Carros])
	case resultado of
		Left err -> return []
		Right ps -> return ps

--geraConsultaCarros                [] =  return [] 
geraConsultaCarros  = do
	listaVeiculos  <- geraConsultaVeiculos
	--let listaIDMarcas   = L.map fst listaTuplaIdMarcas
	--let listaNomeMarcas = L.map snd listaTuplaIdMarcas
	listaCarros       <- sequence( L.map geraListaCarros listaIDMarcas)
	let quantidades     = L.map L.length  listaCarros
	let tripla          = zip3 listaIDMarcas listaNomeMarcas quantidades
	let ordenado        = ordenaCarros tripla
	return (ordenado)

ordenaCarros [] = []
ordenaCarros ls = L.sortBy (compare `on` terceiro) ls

somaTotalCarros :: [( a , b , Int)] -> Int
somaTotalCarros [] = 0
somaTotalCarros ls = L.foldr (+) 0 (L.map terceiro ls)

terceiro :: ( a , b , Int) -> Int
terceiro (_ , _ , x) = x 

segundo :: ( a , b , c) -> b
segundo (_ , x , _) = x 
