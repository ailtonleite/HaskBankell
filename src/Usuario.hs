module Usuario (User(..), addUsuario, removeUsuario, transferirSaldo) where

import Data.List (find, delete)
import Data.Maybe (isJust, fromJust, isNothing)

data User = User
  { userid       :: Integer
  , nome         :: String
  , saldo        :: Float
  , saldoDevedor :: Float
  } deriving (Show, Eq)

-- Verifica se o usuário já existe, caso contrário, adiciona-o à lista
addUsuario :: User -> [User] -> [User]
addUsuario user users
  | isJust (find (\x -> userid x == userid user) users) = users
  | otherwise = user : users

-- Remove usuário
removeUsuario :: Integer -> [User] -> [User]
removeUsuario uid users
  | isJust user = delete (fromJust user) users
  | otherwise = users
  where
    user = find (\x -> userid x == uid) users

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
