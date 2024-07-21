module Main (main) where

import Usuario
import System.IO (hFlush, stdout)

main :: IO ()
main = mainInterface []

printMenu :: IO ()
printMenu = do
  putStrLn "Escolha uma opção:"
  putStrLn "1 - Adicionar usuário"
  putStrLn "2 - Exibir usuários"
  putStrLn "3 - Sair"
  putStrLn "-- HaskBankell - UFABC --"

-- Função de loop principal
mainInterface :: [User] -> IO ()
mainInterface users = do
  printMenu
  putStr "Opção: "
  hFlush stdout
  option <- getLine
  case option of
    "1" -> do
      putStr "codigo identificador do usuário: "
      hFlush stdout
      userid <- readLn :: IO Integer
      putStr "Nome do usuário: "
      hFlush stdout
      nome <- getLine
      putStr "Saldo do usuário: "
      hFlush stdout
      saldo <- readLn :: IO Float
      putStr "Saldo devedor do usuário: "
      hFlush stdout
      saldoDevedor <- readLn :: IO Float
      let newUsers = addUsuario (User userid nome saldo saldoDevedor) users
      putStrLn "Usuário adicionado."
      mainInterface newUsers
    "2" -> do
      putStrLn "Lista de usuários:"
      mapM_ print users
      mainInterface users
    "3" -> do
      putStrLn "Saindo..."
      return ()
    _ -> do
      putStrLn "Erro: Opção invalida"
      mainInterface users

  
  --aqui ficara a interface