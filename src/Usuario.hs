module Usuario (User(..), addUsuario, removeUsuario) where

import Data.List (find, delete)
import Data.Maybe (isJust, fromJust)

data User = User { userid :: Integer, nome :: String, saldo :: Float, saldoDevedor :: Float } deriving (Show, Eq)

-- Verifica se o usuário já existe, caso contrario adicionalo a lista
addUsuario :: User -> [User] -> [User]
addUsuario user users | isJust (find (\x -> userid x == userid user) users) = users
                      | otherwise = user : users

-- remover usuario
removeUsuario :: Integer -> [User] -> [User]
removeUsuario uid users | isJust user = delete (fromJust user) users
                       | otherwise = users
                       where user = find (\x -> userid x == uid) users

-- salvar a lista de usuarios em um arquivo (talvez json)
-- transferir saldo
-- transferir parcelado** (para a segunda parte)