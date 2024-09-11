module Usuario (User(..), addUsuario, removeUsuario, carregaArquivo, salvaUsuario, transferirSaldo, transferirParcelado, pagamentoParcelado) where

import Data.List (find, delete)
import Data.Maybe (isJust, fromJust, isNothing)
import Data.Aeson (ToJSON, FromJSON, encode, decodeStrict')
import qualified Data.ByteString as Byte
import qualified Data.ByteString.Char8 as B
import GHC.Generics (Generic)
import System.Directory (doesFileExist)
import Crypto.Hash (hash, SHA256(..), Digest)

data User = User { userid :: Integer, nome :: String, saldo :: Float, saldoDevedor :: Float, senha :: String } deriving (Show, Eq, Generic)

-- OBS: Para as funções de salvamento e garregamento de dados dos usuarios
-- em um arquivo, foi utilizado a lib aeson na qual seu funcionamento foi
-- estudado via sua doc: https://hackage.haskell.org/package/aeson-2.2.3.0/docs/Data-Aeson.html
-- Alguns exemplos de funcionamento também foram consultados via chatGPT para
-- observar um exemplo de aplicação da lib aeson

instance ToJSON User
instance FromJSON User

hashSenha :: String -> String
hashSenha usenha = show (hash (B.pack usenha) :: Digest SHA256)

-- salvar a lista de usuarios em um arquivo (talvez json)
salvaUsuario :: FilePath -> [User] -> IO ()
salvaUsuario arquivo user = Byte.writeFile arquivo (Byte.toStrict $ encode user)

carregaArquivo :: FilePath -> IO (Maybe [User])
carregaArquivo arquivo = do
    existe <- doesFileExist arquivo
    if not existe
        then return Nothing
        else do
            conteudo <- Byte.readFile arquivo
            return (decodeStrict' conteudo)

-- Verifica se o usuário já existe, caso contrário, adiciona-o à lista
addUsuario :: User -> [User] -> [User]
addUsuario user users
  | isJust (find (\x -> userid x == userid user) users) = users
  | otherwise = user { senha = hashSenha (senha user) } : users

-- Remove usuário
removeUsuario :: Integer -> String -> [User] -> Maybe [User]
removeUsuario uid usenha users
  | isJust user && senhaCorreta = Just $ delete (fromJust user) users
  | otherwise = Nothing
  where
    user = find (\x -> userid x == uid) users
    senhaCorreta = hashSenha usenha == senha (fromJust user)

-- transferir saldo
-- Função para transferir saldo de um usuário para outro
transferirSaldo :: Integer -> String -> Integer -> Float -> [User] -> Either String [User]
transferirSaldo fromId usenha toId amount users
  | amount <= 0 = Left $ "Erro: O valor da transferência deve ser maior que zero."
  | isNothing fromUser = Left $ "Erro: Usuário remetente com ID " ++ show fromId ++ " não encontrado."
  | isNothing toUser = Left $ "Erro: Usuário destinatário com ID " ++ show toId ++ " não encontrado."
  | not (senhaCorreta fromUser) = Left $ "Erro: Senha incorreta para o usuário remetente."
  | saldo (fromJust fromUser) < amount = Left $ "Erro: Saldo insuficiente para transferência."
  | otherwise = Right updatedUsers
  where
    fromUser = find (\x -> userid x == fromId) users
    toUser = find (\x -> userid x == toId) users
    senhaCorreta user = hashSenha usenha == senha (fromJust user)
    updatedUsers = map updateUser users

    updateUser user
      | userid user == fromId = user { saldo = saldo user - amount }
      | userid user == toId = user { saldo = saldo user + amount }
      | otherwise = user

-- transferir parcelado** (para a segunda parte)
transferirParcelado :: Integer -> String -> Integer -> Float -> [User] -> Either String [User]
transferirParcelado fromId usenha toId amount users
  | amount <= 0 = Left $ "Erro: O valor da transferência deve ser maior que zero."
  | isNothing fromUser = Left $ "Erro: Usuário remetente com ID " ++ show fromId ++ " não encontrado."
  | isNothing toUser = Left $ "Erro: Usuário destinatário com ID " ++ show toId ++ " não encontrado."
  | not (senhaCorreta fromUser) = Left $ "Erro: Senha incorreta para o usuário remetente."
  | saldoDevedor (fromJust fromUser) > 0 = Left $ "Erro: Usuário já possui saldo devedor e não poderá realizar uma nova transferencia"
  | otherwise = Right updatedUsers
  where
    fromUser = find (\x -> userid x == fromId) users
    toUser = find (\x -> userid x == toId) users
    senhaCorreta user = hashSenha usenha == senha (fromJust user)
    updatedUsers = map updateUser users

    updateUser user
      | userid user == fromId = user { saldoDevedor = saldoDevedor user + amount }
      | userid user == toId = user { saldo = saldo user + amount }
      | otherwise = user

pagamentoParcelado :: Integer -> String -> Float -> [User] -> Either String [User]
pagamentoParcelado fromId usenha amount users
  | isNothing fromUser = Left $ "Erro: Usuário remetente com ID " ++ show fromId ++ " não encontrado."
  | not (senhaCorreta fromUser) = Left $ "Erro: Senha incorreta para o usuário remetente."
  | saldoDevedor (fromJust fromUser) <= 0 = Left $ "Erro: Usuário não possui saldo devedor em sua conta"
  | amount < valorMinimo fromUser =  Left $ "Erro: Pagamento minimo deve ser no valor de " ++  show (valorMinimo fromUser)
  | otherwise = Right updatedUsers
  where
    fromUser = find (\x -> userid x == fromId) users
    senhaCorreta user = hashSenha usenha == senha (fromJust user)
    valorMinimo user = saldoDevedor (fromJust user) * 0.1
    updatedUsers = map updateUser users

    updateUser user
      | userid user == fromId = user { saldo = saldo user - amount, saldoDevedor = saldoDevedor user - amount }
      | otherwise = user