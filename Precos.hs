
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

data Precos =
	 Precos {  fipe_marcaP  :: String
			  ,referenciaP  :: String
			  ,fipe_codigoP :: String
			  ,nameP        :: String
			  ,marcaP       :: String
			  ,precoP       :: String
			  ,keyP         :: String
			  ,timeP        :: Float  
			  ,idP          :: String
			 }deriving Show
	
instance FromJSON Precos where 
	parseJSON (Object p) =
		  Precos <$> p .: "fipe_marca" 
				 <*> p .: "referencia" 
				 <*> p .: "fipe_codigo"
				 <*> p .: "name"       
				 <*> p .: "marca"
				 <*> p .: "preco"
				 <*> p .: "key"
				 <*> p .: "time"         
				 <*> p .: "id"         
				        
	parseJSON _= mzero 

{--apenas retorna tipo Preco vazio, usado para falha de consultas--}
precoVazio :: Precos
precoVazio = Precos {fipe_marcaP = "vazio", referenciaP = "", fipe_codigoP = "", nameP = "", marcaP = "", precoP = "", keyP = "", timeP = 0.0, idP = ""}

{--Para consulta de precos utilizamos o Id marca -> id veiculos/modelo -> ano 
a consulta retorna p tipo IO (Maybe Preco) --}
getPreco :: Int -> String -> String-> IO (Maybe Precos)
getPreco idMarca idVeiculo ano = fmap decode $ simpleHttp $ "http://fipeapi.appspot.com/api/1/carros/veiculo/"++(show idMarca)++"/"++idVeiculo ++"/"++ano++".json"

{--Precebe uma tripla (Id marca , id veiculos/modelo, ano) e retorna um IO preco
como a consulta e do tipo IO (Maybe Preco), caso a consulta falhe retorna um preco vazio --}
geraListaPrecos :: (Int, String, String) -> IO Precos
geraListaPrecos ltriplaMarcaModeloAno = do
	let msg = ("aguarde... Consulta de precos em andamento... Marca: "++(show (primeiro ltriplaMarcaModeloAno))++" Modelo : "++ (show(segundo ltriplaMarcaModeloAno))++" Ano: "++(show (terceiro ltriplaMarcaModeloAno)) ++"\n") 
	print  msg
	appendFile "log.txt" msg
	p <- getPreco (primeiro ltriplaMarcaModeloAno) (segundo ltriplaMarcaModeloAno) (terceiro ltriplaMarcaModeloAno) 
	case p of
		Nothing -> return precoVazio 
		Just r -> return r  

{--funcoes para retorno de elementos de triplas--}
primeiro :: (a,b,c) -> a
primeiro (a,b,c) = a
segundo  :: (a,b,c) -> b
segundo (a,b,c) = b
terceiro :: (a,b,c) -> c
terceiro (a,b,c) = c


{--recebe uma lista de carros [Carros] e retona uma lista [Precos] com preco de cada
carro da lista [Carro], caso ele exista na tabela fipe--}
geraConsultaPrecos :: [Carros] -> IO [Precos]
geraConsultaPrecos  cs = do
	{-- lista de [(IdMarca,NomeMarca)]--}
	marca <- marcas
	{-- lista de [[Veiculos]]--}
	lVeiculos <- geraConsultaVeiculos
	{-- lista de [[idv,fipe_namev]]--}
	let listaVeiculos = L.foldr (++) [] (L.map (L.map (\v-> [idV v,fipe_nameV v])) lVeiculos)
	{-gera [[fipe_marca= nome marca, veiculo=nome veiculo, fipe_codigo = ano veiculo]]-}
	let ls = L.map (\c -> [fipe_marcaC c, veiculoC c,fipe_codigoC c] ) cs
	{-gera lista [fipe_marca]-}
	let fipeMarca = L.map L.head ls
	{-encontra id das marcas [IdMarcas]-}
	let listaIdMarcas =  L.foldr (++) [] (L.map (\ma -> (L.map fst (L.filter (\tm ->  snd tm ==  ma) marca) ) ) fipeMarca)
	{-gera lista [nomeVeiculos]-}
	let veiculo = L.map (\l -> L.head (L.drop 1 l)) ls
	{-encontra id dos nomeveiculos[idVeiculos]-}
	let listaIdveiculos = L.foldr (++) [] (L.map (\v -> L.map L.head (L.filter (\fp -> (L.head (L.tail fp)) == v) listaVeiculos)) veiculo)
	{-gera lista [anos]-}
	let anos = L.map (\a -> L.head (L.drop 2 a)) ls
	{-gera lista de tripla [(IdMarca, idVeiculo, ano)]-}
	let tripla = L.zip3 listaIdMarcas listaIdveiculos anos
	{-consulta cada marca modelo e ano-> gera [Precos]-}
	listaPrecos <- sequence( L.map geraListaPrecos tripla)
	return (listaPrecos)