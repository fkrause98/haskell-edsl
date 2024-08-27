# haskell-edsl
Repositorio conteniendo el examen del curso de abordaje funcional a los EDSL.

Nombre: Francisco 
Apellido: Krause Arnim
Universidad: Universidad de Buenos Aires
Libreta: 99/19
email: fkrausear@gmail.com

## Estructura

Me tomé la libertad de armar el código como un proyecto de Cabal,
pero también se puede cargar el código en ghci.

### Usando Cabal
Si no se tiene instalado Cabal, se puede descargar con ghcup mediante
el siguiente script:
```sh
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
```
Una vez instalado, se pueden ejecutar los ejemplos en Main.hs con:
```sh
cabal run
```

### Usando GHCI
Alternativamente, si solo tenemos ghc, podemos simplemente hacer:
```sh
ghci app/Main.hs src/Deep.hs src/Shallow.hs src/Parser.hs
```
Una vez en el interprete también podemos correr los ejemplos,
evaluando la función main:
```sh
ghci> main
.... // Output de Main.hs
```

