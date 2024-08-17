module Main (main) where

import Usuario (addUsuario, removeUsuario, User(User), carregaArquivo, salvaUsuario)
import System.IO (hFlush, stdout, IOMode (ReadWriteMode))
import Data.Maybe

main :: IO ()
main = mainInterface []

printMenu :: IO ()
printMenu = do
  putStrLn "Escolha uma opção:"
  putStrLn "1 - Adicionar usuário"
  putStrLn "2 - Exibir usuários"
  putStrLn "2 - Remover usuário"
  putStrLn "4 - Sair"
  putStrLn "-- HaskBankell - UFABC --"

-- aqui ficara a interface
-- OBS:
-- Alguns dos recursos como "mapM" para listagem dos itens da lista, pre-carregamento
-- de usuários ja salvs anterioremnte e readLn foram consultadas via chatGPT
-- mas a ferramenta foi usada apenas para entender e observar seu funcionamento
-- em outros exemplos para poder aplicar em nosso projeto.
mainInterface :: [User] -> IO ()
mainInterface users = do
  maybeUser <- carregaArquivo "./db/User.json"
  let users' = Data.Maybe.fromMaybe [] maybeUser

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
      let newUsers = addUsuario (User userid nome saldo saldoDevedor) users'
      salvaUsuario "./db/User.json" newUsers
      putStrLn "Usuário adicionado."
      mainInterface newUsers

    "2" -> do
      putStrLn "Lista de usuários:"
      mapM_ print users
      mainInterface users

    "3" -> do
      putStr "codigo identificador do usuário a ser removido: "
      hFlush stdout
      userid <- readLn :: IO Integer
      let newUsers = removeUsuario userid users'
      salvaUsuario "./db/User.json" newUsers
      putStrLn "Usuário removido."
      mainInterface newUsers

    "4" -> do

      putStrLn "Saindo..."
      return ()

    _ -> do
      putStrLn "Erro: Opção invalida"
      mainInterface users


  --aqui ficara a interface