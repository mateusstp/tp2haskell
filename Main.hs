import Veiculos
import Marcas
import Carros
import Precos
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Diagrams

{--ordena uma lista  de tuplas com base que o segundo
arqumento tenho uma ordem --}
quicksort :: (Ord b) => [(a,b)] -> [(a,b)]
quicksort [] = []
quicksort (s:xs) = quicksort [x | x <- xs, (snd x) < (snd s)] ++ [s] ++ quicksort [x | x <- xs, (snd x) >= (snd s)]

{--garante que a lista contenha apenas elemento unicos - ou seja a lista
de retorna nao possui duplicacoes -}
unicos :: Eq a => [a] -> [a]
unicos [] = []
unicos (x:xs)
	| elem x  xs =  x : (unicos $ delete x xs)
	| otherwise = x : unicos xs

{--delera um elemento de uma lista-}
delete :: Eq a => a -> [a] -> [a]
delete deleted xs = [ x | x <- xs, x /= deleted ]

{--dado dois inteiros a b retorna a relacao Double 100* y/x--}
percent :: Int -> Int -> Double
percent x y =   100 * ( b / a )
	where 
		  a = fromIntegral x :: Double
		  b = fromIntegral y :: Double

{--dao uma lista de [Double] retorna  a media desse valor--}
mediaD :: [Double] -> Double
mediaD ld = (sum ld) / s
	where
		  a = length ld
		  s = fromIntegral a :: Double

{--converte em preco String -> "R$ 73.458,00" para Float -> 73458.00 
	remove os pontos, troca virgula por ponto,e convete para float--}
precoSToPrecoF :: String -> Float
precoSToPrecoF s = read (map (\c -> if c == ',' then '.'; else c) (filter (\e -> e /='.') (drop 3 s))) :: Float
{--converte em preco String -> "R$ 73.458,00" para Double -> 73458.00 
	remove os pontos, troca virgula por ponto,e convete para double--}
precoSToPrecoD :: String -> Double
precoSToPrecoD s = read (map (\c -> if c == ',' then '.'; else c) (filter (\e -> e /='.') (drop 3 s))) :: Double


{--FUNCOES DA BIBLIOTECA Chart--}
pitem (s,v,o) = pitem_value  .~ v
			  $ pitem_label  .~ s
			  $ pitem_offset .~ (if o then 25 else 0)
			  $ def


--http://revista.pensecarros.com.br/pagina/tipos-de-carros.html
--Modelos  Hatch Compacto para comparacao de precoss
titles :: [String]
titles = ["VW GOl","Fiat Uno","Peugeot 207 ","Chevrolet Celta","Ford Ka"]

--Inicial de nomes de carros cadastrados na tabela fipe
gol   = "Gol"
uno   = "UNO"
celta = "Celta"
ka    = "Ka"
p207  = "207" 
-- anos para analise de depreciacao
cincoanos :: [String]
cincoanos = ["2011","2012", "2013","2014","2015"]

{--recebe um total de carros e uma lista de tuplas [(Nome Marca ,Total de carros marca)]
retorna uma listad de triplas [(Nome Marca, percentual de carros em relacao a total, True)] 
o parametro true indica que o grafico de pizza deve ficar espacado--}
values :: Int -> [(String,Int)] -> [ (String,Double,Bool) ]
values total ls = zip3 (map fst ls) (map (percent total) (map snd ls)) (replicate (length ls) True)

{--recebe uma lista de [[precos de carros]] de tamnho 5 que repesenta as marcas
["VW GOl","Fiat Uno","Peugeot 207 ","Chevrolet Celta","Ford Ka"] e retorna 
uma listra de tupla [(ano, [precos])], a lista de preco ja esta ordenada--}
values5 :: [[Double]] -> [ (String,[Double]) ]
values5 p =  zip cincoanos p 

{--recebe uam lista de precos e retorna 
		String -> Classe de precos
		Double -> porcentagen que cada  clase representa
		Bool   -> apenas afirma para bibloteca para espacar os graficos  
	--}
valuesp :: [Float] -> [ (String,Double,Bool) ]
valuesp ls = filtraCincoClassePrecos ls
{--recebe uam lista de precos e retorna 
		String -> Classe de precos
		Double -> porcentagen que cada  clase representa
		Bool   -> apenas afirma para bibloteca para espacar os graficos  
	--}
filtraCincoClassePrecos :: [Float] -> [(String,Double,Bool)]
filtraCincoClassePrecos lsf =  [(classe1, p1, True),(classe2, p2,True),(classe5, p5,True),(classe3, p3,True),(classe4, p4,True)]
	where 
		lic1 = 20000.00
		lsc1 = 39999.99
		lic2 = 40000.00
		lsc2 = 69999.99
		lic3 = 70000.00
		lsc3 = 99999.99
		lic4 = 100000.00
		lsc4 = 149999.99
		lc5  = 150000.00
		classe1 = "R$20.000,00 a R$39.999,99" 
		classe2 = "R$40.000,00 a R$69.999,99" 
		classe3 = "R$70.000,00 a R$99.999,99" 
		classe4 = "R$100.000,00 a R$149.999,99" 
		classe5 = "mais de R$150.000,00" 
		qdt1  = length (filter (\f -> (f >= lic1) && (f <= lsc1)) lsf)
		qdt2  = length (filter (\f -> (f >= lic2) && (f <= lsc2)) lsf)
		qdt3  = length (filter (\f -> (f >= lic3) && (f <= lsc3)) lsf)
		qdt4  = length (filter (\f -> (f >= lic4) && (f <= lsc4)) lsf)
		qdt5  = length (filter (\f -> f >= lc5 ) lsf)
		total = length lsf
		p1 = percent total qdt1 
		p2 = percent total qdt2
		p3 = percent total qdt3
		p4 = percent total qdt4
		p5 = percent total qdt5

{--gera grafico pizza -> recebe inteiro representando o total de carros 
pesquisados e uma lista contento [(Marca, total de carros marca)] --}
geraGraficoQ1 :: Int -> [(String, Int)] -> IO ()
geraGraficoQ1 total lista = toFile def "Precos.png" $ do
	pie_title .= "Tabbela FIPE - Marcas com mais precos de carro 2013 a 2015"
	pie_plot . pie_data .= map pitem (values total lista)

{--receme o nome de uma marca e uma lista de precos [Float] --}
geraGraficoQ2m1 :: String -> [Float] -> IO ()
geraGraficoQ2m1 marca lista  = toFile def "precosPorfaixas1.png" $ do
	pie_title .= ("Tabbela FIPE : "++marca++" carros faixa de precos - 2013 a 2015")
	{--valuesp :: [Float] -> [ (String,Double,Bool) ] 
		String -> Classe de precos
		Double -> porcentagen que cada  clase representa
		Bool   -> apenas afirma para bibloteca para espacar os graficos  
	--}
	pie_plot . pie_data .= map pitem (valuesp lista)

{-idem geraGraficoQ2m1-}
geraGraficoQ2m2 :: String -> [Float] -> IO ()
geraGraficoQ2m2 marca lista  = toFile def "precosPorfaixas2.png" $ do
	pie_title .= ("Tabbela FIPE : "++marca++" carros faixa de precos - 2013 a 2015")
	pie_plot . pie_data .= map pitem (valuesp lista)
{-idem geraGraficoQ2m1-}
geraGraficoQ2m3 :: String -> [Float] -> IO ()
geraGraficoQ2m3 marca lista  = toFile def "precosPorfaixas3.png" $ do
	pie_title .= ("Tabbela FIPE : "++marca++" carros faixa de precos - 2013 a 2015")
	pie_plot . pie_data .= map pitem (valuesp lista)
{-idem geraGraficoQ2m1-}
geraGraficoQ2m4 :: String -> [Float] -> IO ()
geraGraficoQ2m4 marca lista  = toFile def "precosPorfaixas4.png" $ do
	pie_title .= ("Tabbela FIPE : "++marca++" carros faixa de precos - 2013 a 2015")
	pie_plot . pie_data .= map pitem (valuesp lista)
{-idem geraGraficoQ2m1-}
geraGraficoQ2m5 :: String -> [Float] -> IO ()
geraGraficoQ2m5 marca lista  = toFile def "precosPorfaixas5.png" $ do
	pie_title .= (("Tabbela FIPE : "++marca++" carros faixa de precos - 2013 a 2015"))
	pie_plot . pie_data .= map pitem (valuesp lista)

{--rebece uma lista de medias de precos de tamanho 5 quue representa os ultimos cinco anos
Estrutura: [[media preco 2011,media preco 2012,media preco 2013,media preco 2014,media preco 2015]]
e gera um grafico na forma de histograma
 --}
geraGraficoQ3 :: [[Double]] -> IO ()
geraGraficoQ3 listaDouble = toFile def "depreciacao.png" $ do
    layout_title .= "Depreciacao Media em 5 anos - Modelos Hatch Compacto"
    layout_title_style . font_size .= 10
    layout_x_axis . laxis_generate .= autoIndexAxis (map fst (values5 listaDouble))
    plot $ fmap plotBars $ bars titles (addIndexes (map snd (values5 listaDouble)))

{--recebe uma lista de precos pre-processada dos anos 2013 a 2015--}
main20131415 :: [Precos] -> IO ()
main20131415 listaPrecos20131415 = do
	{--*********Faz consulta sobre lista de precos processa da tabela fipe ***********---}
	{-- gera [fipe_marca == nome da marca]--}
	let lmarcas       = map fipe_marcaP listaPrecos20131415
	{-- remove repeticos da [fipe_marca == nome da marca]--}
	let lmarcasUnicas = unicos lmarcas
	{-- [fipe_marca == nome da marca]--}
	let contMarcas    = zip lmarcasUnicas (map length (map (\m -> filter (\cm -> m == cm) lmarcas) lmarcasUnicas))
	{--lista tupla 5 marcas mais precos [(string=nome marca,int=quantidade carros)]--}
	let cincomais = take 5 $ reverse $ quicksort contMarcas
	print "Gerando graficos questao 1..."
	{--gera graifico quenstao 1 cinco marcas com mais precos listados--}
	geraGraficoQ1 (length lmarcas) cincomais
	print "Gerando graficos questao 2..."
	{--******infromacoes questao 2--}
	{--lista 5 marcas mais precos [string]--}
	let m5 = map fst cincomais
	{-filtrsa precos cinco marcas-}
	let filtramarcas = map (\m -> filter (\mc -> (fipe_marcaP mc) == m ) listaPrecos20131415) m5
	{--gera tupla [(Strgin = Marca,Float = preco)]--}
	let tmpreco = map (map (\p -> (fipe_marcaP p, precoSToPrecoF (precoP p)) )) filtramarcas 
	{--gera lista precos [[preco==float])]--}
	let lprecos = map (map snd) tmpreco
	let lf =  zip m5 lprecos
	{--gera graficos--}
	let (m1:ms) = lf
	geraGraficoQ2m1 (fst m1) (snd m1)
	let (m2:ms2) = ms
	geraGraficoQ2m2 (fst m2) (snd m2)
	let (m3:ms3) = ms2
	geraGraficoQ2m3 (fst m3) (snd m3)
	let (m4:ms4) = ms3
	geraGraficoQ2m4 (fst m4) (snd m4)
	let (m5:ms5) = ms4
	geraGraficoQ2m5 (fst m5) (snd m5)
	
{--recebe uma lista de precos pre-processada dos anos 2011 a 2015--}
main201112131415 :: [Precos] -> IO ()
main201112131415  listaPrecos201112131415 = do
	{--*********Faz consulta sobre lista de precos processa da tabela fipe ***********---}
	{--gera listra de string fd preco--}
	print "Gerando graficos questao 3..."
	{--filtra precos para carro gol--}
	let pgol2011  = map precoSToPrecoD $ map precoP (filter (\p -> (take 3 (nameP p)) == gol && (fipe_codigoP p) == "2011") listaPrecos201112131415)
	let pgol2012  = map precoSToPrecoD $ map precoP (filter (\p -> (take 3 (nameP p)) == gol && (fipe_codigoP p) == "2012") listaPrecos201112131415)
	let pgol2013  = map precoSToPrecoD $ map precoP (filter (\p -> (take 3 (nameP p)) == gol && (fipe_codigoP p) == "2013") listaPrecos201112131415)
	let pgol2014  = map precoSToPrecoD $ map precoP (filter (\p -> (take 3 (nameP p)) == gol && (fipe_codigoP p) == "2014") listaPrecos201112131415)
	let pgol2015  = map precoSToPrecoD $ map precoP (filter (\p -> (take 3 (nameP p)) == gol && (fipe_codigoP p) == "2015") listaPrecos201112131415)
	{--gera media de precos para carro gol--}
	let ptgol =[mediaD pgol2011, mediaD pgol2012, mediaD pgol2013, mediaD pgol2014, mediaD pgol2015] 
	{--filtra precos para carro ka--}
	let pka2011  = map precoSToPrecoD $ map precoP (filter (\p -> (take 2 (nameP p)) == ka && (fipe_codigoP p) == "2011") listaPrecos201112131415)
	let pka2012  = map precoSToPrecoD $ map precoP (filter (\p -> (take 2 (nameP p)) == ka && (fipe_codigoP p) == "2012") listaPrecos201112131415)
	let pka2013  = map precoSToPrecoD $ map precoP (filter (\p -> (take 2 (nameP p)) == ka && (fipe_codigoP p) == "2013") listaPrecos201112131415)
	let pka2014  = map precoSToPrecoD $ map precoP (filter (\p -> (take 2 (nameP p)) == ka && (fipe_codigoP p) == "2014") listaPrecos201112131415)
	let pka2015  = map precoSToPrecoD $ map precoP (filter (\p -> (take 2 (nameP p)) == ka && (fipe_codigoP p) == "2015") listaPrecos201112131415)
	{--gera media de precos para carro ka--}
	let ptka =[mediaD pka2011, mediaD pka2012, mediaD pka2013, mediaD pka2014, mediaD pka2015]
	{--filtra precos para carro celta--}
	let pcelta2011  = map precoSToPrecoD $ map precoP (filter (\p -> (take 5 (nameP p)) == celta && (fipe_codigoP p) == "2011") listaPrecos201112131415)
	let pcelta2012  = map precoSToPrecoD $ map precoP (filter (\p -> (take 5 (nameP p)) == celta && (fipe_codigoP p) == "2012") listaPrecos201112131415)
	let pcelta2013  = map precoSToPrecoD $ map precoP (filter (\p -> (take 5 (nameP p)) == celta && (fipe_codigoP p) == "2013") listaPrecos201112131415)
	let pcelta2014  = map precoSToPrecoD $ map precoP (filter (\p -> (take 5 (nameP p)) == celta && (fipe_codigoP p) == "2014") listaPrecos201112131415)
	let pcelta2015  = map precoSToPrecoD $ map precoP (filter (\p -> (take 5 (nameP p)) == celta && (fipe_codigoP p) == "2015") listaPrecos201112131415)
	{--gera media de precos para carro celta--}
	let ptcelta =[mediaD pcelta2011, mediaD pcelta2012, mediaD pcelta2013, mediaD pcelta2014, mediaD pcelta2015]	
	{--filtra precos para carro uno--}
	let puno2011  = map precoSToPrecoD $ map precoP (filter (\p -> (take 3 (nameP p)) == uno && (fipe_codigoP p) == "2011") listaPrecos201112131415)
	let puno2012  = map precoSToPrecoD $ map precoP (filter (\p -> (take 3 (nameP p)) == uno && (fipe_codigoP p) == "2012") listaPrecos201112131415)
	let puno2013  = map precoSToPrecoD $ map precoP (filter (\p -> (take 3 (nameP p)) == uno && (fipe_codigoP p) == "2013") listaPrecos201112131415)
	let puno2014  = map precoSToPrecoD $ map precoP (filter (\p -> (take 3 (nameP p)) == uno && (fipe_codigoP p) == "2014") listaPrecos201112131415)
	let puno2015  = map precoSToPrecoD $ map precoP (filter (\p -> (take 3 (nameP p)) == uno && (fipe_codigoP p) == "2015") listaPrecos201112131415)
	{--gera media de precos para carro uno--}
	let ptuno =[mediaD puno2011, mediaD puno2012, mediaD puno2013, mediaD puno2014, mediaD puno2015]	
	{--filtra precos para carro peugeot 207--}
	let pp2072011  = map precoSToPrecoD $ map precoP (filter (\p -> (take 3 (nameP p)) == p207 && (fipe_codigoP p) == "2011") listaPrecos201112131415)
	let pp2072012  = map precoSToPrecoD $ map precoP (filter (\p -> (take 3 (nameP p)) == p207 && (fipe_codigoP p) == "2012") listaPrecos201112131415)
	let pp2072013  = map precoSToPrecoD $ map precoP (filter (\p -> (take 3 (nameP p)) == p207 && (fipe_codigoP p) == "2013") listaPrecos201112131415)
	let pp2072014  = map precoSToPrecoD $ map precoP (filter (\p -> (take 3 (nameP p)) == p207 && (fipe_codigoP p) == "2014") listaPrecos201112131415)
	let pp2072015  = map precoSToPrecoD $ map precoP (filter (\p -> (take 3 (nameP p)) == p207 && (fipe_codigoP p) == "2015") listaPrecos201112131415)
	{--gera media de precos para carro peugeot 207--}
	let ptp207 =[mediaD pp2072011, mediaD pp2072012, mediaD pp2072013, mediaD pp2072014, mediaD pp2072015]	
	--["VW GOl","Fiat Uno","Peugeot 207 ","Chevrolet Celta","Ford Ka"]
	-- lista de media de calos 
	--[[media preco 2011,media preco 2012,media preco 2013,media preco 2014,media preco 2015]...ka]
	geraGraficoQ3 [ptgol,ptuno,ptp207,ptcelta,ptka]
	

main = do
	print "Inicianco consultas"
	{--*********Faz consulta sobre lista de carros processa da tabela fipe ***********---}
	print "Gerando consulta de todos os carros... Isso pode demorar algumas horas"
	{-gera [[[Carros]]]-}
	carros <- geraConsultaCarros
	print "Consulta carro finalizada..."
	print "Filtrando carro 2011  a 2015..."
	{--filtra carros de 2011 a 2015--}
	let carros2011_15 = map (map (filter (\c -> (idC c == "2011-3" ||idC c == "2011-1" ||idC c == "2012-3" ||idC c == "2012-1" || idC c == "2015-1" || idC c == "2015-3" || idC c == "2013-1" || idC c == "2013-3" || idC c == "2014-1" || idC c == "2014-3" )))) carros 
	{-concatena lista  -> [[[Carros]]] -> [Carros]-}	
	let carfiltrados = foldr (++) [] (foldr (++) [] carros2011_15 )
	print "Iniciada consulta precos 2011  a 2015..."
	{--retorna lista de [Precos]--}
	l1 <- geraConsultaPrecos carfiltrados
	print "Terminada consulta de precos 2011  a 2015..."
	print "Filtrando precos de 2013  a 2015..."
	{--filtra caros de 2013 a 2015--}
	let l2  = filter (\p -> (fipe_codigoP p == "2015" || fipe_codigoP p == "2013" || fipe_codigoP p == "2014" )) l1
	print "Terminada filtragem de precos de 2013  a 2015..."
	print "Inicado proceso de contrucao de graficos..."
	{--invoca funcoes para gerar graficos--}
	main20131415  l2
	main201112131415 l1
	print "Fim execucao..."