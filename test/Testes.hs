{-# LANGUAGE OverloadedStrings #-}
module Main where

import Test.HUnit
import Usuario

-- Funcoes de teste

-- Teste addUsuario
testAddUsuario :: Test
testAddUsuario = TestCase $ do
    let user1 = User 1 "Ana" 100.0 0.0
    let user2 = User 2 "Joana" 50.0 0.0
    let users = [user1]
    let newUsers = addUsuario user2 users
    assertEqual "Usuário não foi adicionado" [user2, user1] newUsers

-- Teste removeUsuario
testRemoveUsuario :: Test
testRemoveUsuario = TestCase $ do
    let user1 = User 1 "Ana" 100.0 0.0
    let user2 = User 2 "Joana" 50.0 0.0
    let users = [user1, user2]
    let newUsers = removeUsuario 1 users
    assertEqual "Usuário não foi removido" [user2] newUsers

-- Teste do transferirSaldo
testTransferirSaldo :: Test
testTransferirSaldo = TestCase $ do
    let user1 = User 1 "Ana" 100.0 0.0
    let user2 = User 2 "Joana" 50.0 0.0
    let users = [user1, user2]
    let result = transferirSaldo 1 2 30.0 users
    case result of
        Left err -> assertFailure err
        Right updatedUsers -> do
            let expectedUser1 = User 1 "Ana" 70.0 0.0
            let expectedUser2 = User 2 "Joana" 80.0 0.0
            assertEqual "Saldo incorreto para usuário 1" expectedUser1 (head updatedUsers)
            assertEqual "Saldo incorreto para usuário 2" expectedUser2 (updatedUsers !! 1)

-- Teste transferir saldo com valor negativo
testTransferirSaldoNegative :: Test
testTransferirSaldoNegative = TestCase $ do
    let user1 = User 1 "Ana" 100.0 0.0
    let user2 = User 2 "Joana" 50.0 0.0
    let users = [user1, user2]
    let result = transferirSaldo 1 2 (-30.0) users
    assertEqual "Deveria retornar erro para valor negativo" (Left "Erro: O valor da transferência deve ser maior que zero.") result

-- Teste transferir saldo para usuário inexistente
testTransferirSaldoUserNotFound :: Test
testTransferirSaldoUserNotFound = TestCase $ do
    let user1 = User 1 "Ana" 100.0 0.0
    let users = [user1]
    let result = transferirSaldo 1 2 30.0 users
    assertEqual "Deveria retornar erro para usuário destinatário não encontrado" (Left "Erro: Usuário destinatário com ID 2 não encontrado.") result


main :: IO ()
main = do
    _ <- runTestTT $ TestList [testAddUsuario, testRemoveUsuario, testTransferirSaldo, testTransferirSaldoNegative, testTransferirSaldoUserNotFound]
    return ()

