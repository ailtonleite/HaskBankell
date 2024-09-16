import Test.HUnit
import Usuario (User(..), addUsuario, removeUsuario, transferirSaldo)

-- Testa a adição de um usuario
testAddUsuario :: Test
testAddUsuario = TestCase $ do
    let usuarios = []
    let usuario = User 1 "Luciano" 100.0 0.0 ""  
    let usuariosAtualizados = addUsuario usuario usuarios
    assertEqual "Deve adicionar um usuário à lista" [usuario] usuariosAtualizados

-- Testa a remoçao de um usuário
testRemoveUsuario :: Test
testRemoveUsuario = TestCase $ do
    let usuario = User 1 "Luciano" 100.0 0.0 ""  
    let usuarios = [usuario]
    let usuariosAtualizados = removeUsuario 1 "" usuarios
    assertEqual "Deve remover o usuário da lista" Nothing usuariosAtualizados

-- Testa a transferencia de saldo
testTransferirSaldo :: Test
testTransferirSaldo = TestCase $ do
    let usuario1 = User 1 "Luciano" 100.0 0.0 ""  
    let usuario2 = User 2 "Ana" 50.0 0.0 ""  
    let usuarios = [usuario1, usuario2]
    let resultado = transferirSaldo 1 "" 2 30.0 usuarios
    case resultado of
        Right usuariosAtualizados -> do
            let usuario1Atualizado = User 1 "Luciano" 70.0 0.0 ""  
            let usuario2Atualizado = User 2 "Ana" 80.0 0.0 ""  
            assertEqual "O saldo do usuário 1 deve ser 70.0" usuario1Atualizado (head usuariosAtualizados)
            assertEqual "O saldo do usuário 2 deve ser 80.0" usuario2Atualizado (usuariosAtualizados !! 1)
        Left msg -> assertFailure ("Falha na transferência: " ++ msg)

-- Testa a adição de usuario duplicado (se há)
testAddUsuarioDuplicado :: Test
testAddUsuarioDuplicado = TestCase $ do
    let usuario = User 1 "Luciano" 100.0 0.0 ""  
    let usuarios = [usuario]
    let usuariosAtualizados = addUsuario usuario usuarios
    assertEqual "Não deve adicionar usuário duplicado" [usuario] usuariosAtualizados

-- Testa a remoção de usuário inexistente
testRemoveUsuarioInexistente :: Test
testRemoveUsuarioInexistente = TestCase $ do
    let usuario = User 1 "Luciano" 100.0 0.0 ""  
    let usuarios = [usuario]
    let usuariosAtualizados = removeUsuario 2 "" usuarios
    assertEqual "Não deve remover um usuário que não existe" (Just [usuario]) usuariosAtualizados

main :: IO ()
main = do
    runTestTT testAddUsuario
    runTestTT testRemoveUsuario
    runTestTT testTransferirSaldo
    runTestTT testAddUsuarioDuplicado
    runTestTT testRemoveUsuarioInexistente
    return ()

