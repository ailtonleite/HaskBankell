module Usuario (User(..), addUsuario) where

import Data.List (find)
import Data.Maybe (isJust)

data User = User { userid :: Integer, nome :: String, saldo :: Float, saldoDevedor :: Float } deriving (Show, Eq)

-- Verifica se o usuário já existe, caso contrario adicionalo a lista
addUsuario :: User -> [User] -> [User]
addUsuario user users | isJust (find (\x -> userid x == userid user) users) = users
                      | otherwise = user : users

-- remover usuario
-- salvar a lista de usuarios em um arquivo (talvez json)
-- transferir saldo
-- transferir parcelado