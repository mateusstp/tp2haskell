import Veiculos
import Marcas
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Diagrams


--values :: [ (String,Double,Bool) ]
--values = [ ("Mexico City",19.2,True), ("Mumbai",12.9,True), ("Sydney",4.3,True), ("London",8.3,True), ("New York",8.2,True) ]


pitem (s,v,o) = pitem_value  .~ v
              $ pitem_label  .~ s
              $ pitem_offset .~ (if o then 25 else 0)
              $ def


values :: Int -> [(a,String,Int)] -> [ (String,Double,Bool) ]
values total ls = zip3 (map segundo ls) (map (percent total) (map terceiro ls)) (replicate (length ls) True)
        
        



percent :: Int -> Int -> Double
percent x y =   100 * ( b / a )
    where 
          a = fromIntegral x :: Double
          b = fromIntegral y :: Double

main = do
    listaMarcas <- marcas
    consulta <- geraConsulta listaMarcas
    print "*********************************************"
    print consulta
    let cincoMais = take 5 (reverse consulta)
    print "*********************************************"
    print cincoMais
    let totalCarros = somaTotalCarros consulta
    print "*********************************************"
    print totalCarros
    print "*********************************************"
    let lva = values totalCarros cincoMais
    print "*********************************************"
    geraGraficoQ1 totalCarros cincoMais


geraGraficoQ1 total lista = toFile def "questao1.png" $ do
    pie_title .= "Marcas com mais carros"
    pie_plot . pie_data .= map pitem (values total lista)
{-main = toFile def "example5_big.png" $ do
  pie_title .= "Relative Population"
  pie_plot . pie_data .= map pitem values-}
