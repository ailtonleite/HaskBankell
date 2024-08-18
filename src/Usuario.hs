module Usuario (User(..), addUsuario, removeUsuario, carregaArquivo, salvaUsuario, transferirSaldo) where

import Data.List (find, delete)
import Data.Maybe (isJust, fromJust, isNothing)
import Data.Aeson (ToJSON, FromJSON, encode, decodeStrict')
--import qualified Data.ByteString.Lazy as Byte
import qualified Data.ByteString as Byte
import GHC.Generics (Generic)
import System.Directory (doesFileExist)

data User = User { userid :: Integer, nome :: String, saldo :: Float, saldoDevedor :: Float } deriving (Show, Eq, Generic)

-- OBS: Para as funções de salvamento e garregamento de dados dos usuarios
-- em um arquivo, foi utilizado a lib aeson na qual seu funcionamento foi
-- estudado via sua doc: https://hackage.haskell.org/package/aeson-2.2.3.0/docs/Data-Aeson.html
-- Alguns exemplos de funcionamento também foram consultados via chatGPT para
-- observar um exemplo de aplicação da lib aeson

instance ToJSON User
instance FromJSON User

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
  | otherwise = user : users

-- Remove usuário
removeUsuario :: Integer -> [User] -> [User]
removeUsuario uid users | isJust user = delete (fromJust user) users
                        | otherwise = users
                        where user = find (\x -> userid x == uid) users

-- transferir saldo
-- Função para transferir saldo de um usuário para outro
transferirSaldo :: Integer -> Integer -> Float -> [User] -> Either String [User]
transferirSaldo fromId toId amount users
  | amount <= 0 = Left $ "Erro: O valor da transferência deve ser maior que zero."
  | isNothing fromUser = Left $ "Erro: Usuário remetente com ID " ++ show fromId ++ " não encontrado."
  | isNothing toUser = Left $ "Erro: Usuário destinatário com ID " ++ show toId ++ " não encontrado."
  | saldo (fromJust fromUser) < amount = Left $ "Erro: Saldo insuficiente para transferência."
  | otherwise = Right updatedUsers
  where
    fromUser = find (\x -> userid x == fromId) users
    toUser = find (\x -> userid x == toId) users
    updatedUsers = map updateUser users

    updateUser user
      | userid user == fromId = user { saldo = saldo user - amount }
      | userid user == toId = user { saldo = saldo user + amount }
      | otherwise = user

-- transferir parcelado** (para a segunda parte)
