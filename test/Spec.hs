import PdePreludat
import Library
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "mandarAlAnio" $ do
    it "mandarAlAnio" $ do
      mandarAlAnio 400 aku `shouldBe` UnPersonaje {nombre = "Aku", salud = 500.0, elementos = [UnElemento {tipo = "Maldad", ataque = <una función>, defensa = <una función>},UnElemento {tipo = "Proteccion", ataque = <una función>, defensa = <una función>}], anioPresente = 400.0}

