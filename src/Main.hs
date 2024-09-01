module Main (main) where

import Usuario (addUsuario, removeUsuario, User(User), carregaArquivo, salvaUsuario, transferirSaldo)
import System.IO (hFlush, stdout)
import Data.Maybe
import Text.Read (readMaybe)

main :: IO ()
main = mainInterface []

printMenu :: IO ()
printMenu = do
  putStrLn "Escolha uma opção:"
  putStrLn "1 - Adicionar usuário"
  putStrLn "2 - Exibir usuários"
  putStrLn "3 - Remover usuário"
  putStrLn "4 - Transferir saldo"
  putStrLn "5 - Sair"
  putStrLn "-- HaskBankell - UFABC --"
  putStrLn " "

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
      putStr "Código identificador do usuário: "
      hFlush stdout
      useridInput <- getLine
      let userid = readMaybe useridInput :: Maybe Integer
      case userid of
        Nothing -> do
          putStrLn "Erro: Código identificador inválido. Deve ser um número inteiro."
          mainInterface users
        Just uid -> do
          putStr "Nome do usuário: "
          hFlush stdout
          nome <- getLine
          putStr "Saldo do usuário: "
          hFlush stdout
          saldoInput <- getLine
          let saldo = readMaybe saldoInput :: Maybe Float
          case saldo of
            Nothing -> do
              putStrLn "Erro: Saldo inválido. Deve ser um número."
              mainInterface users
            Just s -> do
              putStr "Saldo devedor do usuário: "
              hFlush stdout
              saldoDevedorInput <- getLine
              let saldoDevedor = readMaybe saldoDevedorInput :: Maybe Float
              case saldoDevedor of
                Nothing -> do
                  putStrLn "Erro: Saldo devedor inválido. Deve ser um número."
                  mainInterface users
                Just sd -> do
                  putStr "Senha do usuario: "
                  hFlush stdout
                  senhaInput <- getLine
                  let newUsers = addUsuario (User uid nome s sd senhaInput) users'
                  salvaUsuario "./db/User.json" newUsers
                  putStrLn "Usuário adicionado."
                  putStrLn " "
                  mainInterface newUsers
                  
    "2" -> do
      putStrLn "Lista de usuários:"
      mapM_ print users
      putStrLn " "
      mainInterface users

    "3" -> do
      putStr "Código identificador do usuário a ser removido: "
      hFlush stdout
      userid <- readLn :: IO Integer
      putStr "Senha do usuario para confirmacao: "
      hFlush stdout
      usenha <- getLine :: IO String
      let newUsers = removeUsuario userid usenha users'
      case newUsers of
        Nothing -> do
          putStrLn "Erro: Usuário não existe ou senha invalida!"
          putStrLn " "
          mainInterface users
        Just nu -> do
          salvaUsuario "./db/User.json" nu
          putStrLn "Usuário removido."
          putStrLn " "
          mainInterface nu
          

    "4" -> do
      putStr "Código identificador do usuário remetente: "
      hFlush stdout
      fromId <- readLn :: IO Integer
      putStr "Código identificador do usuário destinatário: "
      hFlush stdout
      toId <- readLn :: IO Integer
      putStr "Valor a ser transferido: "
      hFlush stdout
      amount <- readLn :: IO Float
      putStr "Senha do usuario remetente para confirmacao: "
      hFlush stdout
      usenha <- getLine :: IO String
      let result = transferirSaldo fromId usenha toId amount users
      case result of
        Left err -> do
          putStrLn err
          mainInterface users
        Right newUsers -> do
          salvaUsuario "./db/User.json" newUsers
          putStrLn "Transferência realizada com sucesso."
          putStrLn " "
          mainInterface newUsers

    "5" -> do
      putStrLn "Saindo..."
      return ()

    _ -> do
      putStrLn "Erro: Opção inválida"
      putStrLn " "
      mainInterface users
