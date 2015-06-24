
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

{--data Carros =
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
--}

getJSONCarros :: String -> String -> IO B.ByteString
getJSONCarros idMarca idVeiculo = simpleHttp $ "http://fipeapi.appspot.com/api/1/carros/veiculo/"++idMarca++"/"++idVeiculo++".json"

{--geraListaCarros = do 
	resultado <- (eitherDecode <$> (getJSONCarros (fst tuplaIdmIdv) (snd tuplaIdmIdv) ) :: IO (Either String [Carros])
	case resultado of
		Left err -> return []
		Right ps -> return ps--}

{-buscaIdVeiculos :: String -> [Veiculos] -> [String]
buscaIdVeiculos marca listaVeiculos = L.map idV (L.filter (\y -> marcaV y == marca ) listaVeiculos)-}

buscaModelos :: [Veiculos] -> [(String, String)]
buscaModelos listaVeiculos = L.zip (L.map idV listaVeiculos) (L.map marcaV listaVeiculos)

{-buscaIdVeiculos :: [(String,String)] -> [(Int,String)]
buscaIdVeiculos lista = L.map idV (L.filter (\y -> marcaV y == marca ) listaVeiculos)-}

{--buscaIds :: [(Int, String)] -> [(String, String)] -> [(Int, String)]
--buscaIds tim tidm = L.map (\m -> map (L.filter (\tp -> (snd tp) == m ))  tuplaIdvMarca) (L.map snd tim)
--buscaIds :: (Int, String) -> [(String, String)] -> [(Int, String)]
buscaIds tim tidm = (L.map (\m -> zip (replicate (length fx) fst m) fx ) listaTuplaIdMarcas)
	where
		let fx = map fst (map (L.filter (\tp -> (snd tp) == (snd m )))  tuplaIdvMarca)--}


aplicaMarca :: [Int] -> [[String]] -> [[(Int, String)]]
aplicaMarca [] _ = []
aplicaMarca _ [] = []
aplicaMarca (id:ids) (l:ls) = L.zip (L.replicate (L.length l) id) l : aplicaMarca ids ls 

 --map (\li -> map (\idm ->(aplicaMarca idm) li ) m) con--

--L.map (\idm -> map (\li -> (aplicaMarca idm) li) ls) m

{--geraConsultaCarros  = do
	{--gera tupla(ID,MARCA)--}
	listaTuplaIdMarcas  <- marcas
	let listaMarca = L.map snd listaTuplaIdMarcas
	{--gera lista de todos [[Veiculos]] --}
	listaVeiculos   <- geraConsultaVeiculos
	{--gera lista de todos [[(Idv,Idm)]] --}
	let tuplaIdvMarca = L.map buscaModelos listaVeiculos 
	
	let tuplaIdmIdv   = L.map (\m -> map fst (map (L.filter (\tp -> (snd tp) == m ))  tuplaIdvMarca)) (L.map snd listaTuplaIdMarcas)
	L.map (\m -> zip ( map fst (L.filter (\tp -> (snd tp) == m ))  tuplaIdvMarca) (L.map snd listaTuplaIdMarcas)

	return tuplaIdvIdm
--}

	{--gera lista de todos [IDVeiculos] --}
	--let listaIdVeiculos   =  foldl (\a b -> a ++ b) [] (L.map (map idV) listaVeiculos)
	{--gera lista de todos [IDVeiculos] --}
	
{--ordenaCarros [] = []
ordenaCarros ls = L.sortBy (compare `on` terceiro) ls

somaTotalCarros :: [( a , b , Int)] -> Int
somaTotalCarros [] = 0
somaTotalCarros ls = L.foldr (+) 0 (L.map terceiro ls)

terceiro :: ( a , b , Int) -> Int
terceiro (_ , _ , x) = x 

segundo :: ( a , b , c) -> b
segundo (_ , x , _) = x --}
