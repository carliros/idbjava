-- | Modulo Main, la interfaz de usuario
module Main where

import Graphics.UI.WXCore
import Graphics.UI.WX
import qualified Control.Exception as C

import Jvm.Data.ClassFormat
import Jvm.BinaryClass
import Jvm.PrettyClass
import qualified UU.Pretty as P

import System.Cmd
import System.Exit

import Data.Char

about :: String
about = "dbJava GUI Interface, desarrollado en el San Simon Haskell Hackathone, Abril 2011"

-- | Path a la carpeta temporal
tmpPath = "./tmp/"

-- | Main
main :: IO ()
main = start gui

-- | la funcion que se encargar de dibujar todo
gui ::IO()
gui = 
    do f <- frame [text := "Interface DBJAVA"]
       eSrc <- entry  f [text := "Nombre del archivo java a generar"]
       bSlf <- button f [text := "Seleccionar"]
       bGen <- button f [text := "Generar ByteCode"]
       spl  <- splitterWindow f []
       pnl1 <- textCtrl spl [wrap := WrapNone, font := fontFixed]
       pnl2 <- textCtrl spl [wrap := WrapNone, font := fontFixed]
       crearMenus f eSrc pnl1
       set bSlf [on command  := selectSource f eSrc pnl1]
       set eSrc [on enterKey := abrirSource f eSrc pnl1]
       set bGen [on command  := generarByteCode f pnl1 pnl2]
       splitterWindowSplitVertically spl pnl1 pnl2 0
       set f [ layout := column 5 [ row 5 [hfill $ widget eSrc, widget bSlf, widget bGen]
                                  , fill $ widget spl]
             ]

-- | generar Byte Code
generarByteCode f pnl1 pnl2
    = catch generar error
    where generar = do cnt <- get pnl1 text
                       if esVacio cnt
                        then C.throw $ userError "El contenido es vacio"
                        else do nclass <- obtenerNombreClase cnt
                                obj    <- decodeClassFile nclass
                                set pnl2 [text := P.disp (P.pp obj) 100 ""]
          error = errorDialog f "Error" . show
          esVacio = null . words

-- | obtener el nombre una clase
obtenerNombreClase :: String -> IO String
obtenerNombreClase cnt
    = catch callJava error
    where error e = print e >> return ""
          callJava = do let nmJava = obtenerNombreJava cnt
                            jvPath = tmpPath ++ nmJava ++ ".java"
                            clPath = tmpPath ++ nmJava ++ ".class"
                        writeFile jvPath cnt
                        ec <- system ("javac " ++ jvPath ++ " -d " ++ tmpPath)
                        case ec of
                            ExitFailure _ -> C.throw (userError "Ocurrio un error al generar el .class") >> return ""
                            ExitSuccess   -> do putStrLn $ "Se genero el archivo " ++ clPath
                                                return clPath

obtenerNombreJava :: String -> String
obtenerNombreJava = nombre . dropWhile (/= "class") . words
    where nombre (c:n:_) = let nm = takeWhile (not . isPunctuation) n
                           in if null nm 
                              then C.throw (userError "La clase java no tiene un nombre")
                              else nm
          nombre _       = C.throw (userError "La clase java no tiene un nombre")


-- | seleccionar un archivo java
selectSource f eSrc pnl1
    = catch select_open error
    where setSource javafile = do set eSrc [text := javafile]
                                  abrirSource f eSrc pnl1
          nothing            = return ()
          error = errorDialog f "Error" . show
          select_open = do mfile <- fileOpenDialog f True True "Abrir Archivo" [("Java File", ["*.java"])] "" ""
                           maybe nothing setSource mfile

-- | arbir un archivo java
abrirSource f eSrc pnl1
    = catch abrir error
    where abrir = do nfile <- get eSrc text
                     cnt   <- readFile nfile
                     set pnl1 [text := cnt]
          error = errorDialog f "Error" . show

-- | crea los menus del frame
crearMenus f eSrc pnl1
    = do pfile <- menuPane [text := "&Archivo"]
         menuItem pfile [ text := "Abrir\tCtrl+o"
                        , on command := selectSource f eSrc pnl1]
         menuLine pfile
         menuQuit pfile [ text := "&Cerrar\tCtrl+C"
                        , on command := close f]
         pabout <- menuPane [text := "About"]
         menuAbout pabout [ text := "About"
                          , on command := infoDialog f "About" about]
         set f [menuBar := [pfile, pabout]]

