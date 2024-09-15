module Main (main) where

import Usuario (addUsuario, removeUsuario, User(User), carregaArquivo, salvaUsuario, transferirSaldo, transferirParcelado, pagamentoParcelado)
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Text.Read (readMaybe)
import Data.Maybe (fromMaybe)
import System.Exit (exitSuccess)

-- Funcao auxiliar para obter o ID de um usuario
getUserId :: User -> Integer
getUserId (User uid _ _ _ _) = uid

-- Estado da aplicacao
data AppState = AppState
  { menuOption     :: Int
  , users          :: [User]
  , inputText      :: String
  , currentState   :: State
  , tempUserId     :: Maybe Integer
  , tempReceiverId :: Maybe Integer
  , tempUserName   :: String
  , tempUserSaldo  :: Maybe Float
  , transferValue  :: Maybe Float
  , successMessage :: String
  , delayTime      :: Float
  }

data State = MainMenu | AddUserID | AddUserName | AddUserSaldo | RemoveUser | TransferSaldo | TransferSenderID | TransferValue | TransferReceiverID | TransferParceladoSenderID | TransferParceladoValue | TransferParceladoReceiverID | PagamentoParceladoID | PagamentoParceladoValue | ExibirUsuarios | ExitApp | SuccessMessageState | QuitState deriving (Eq)

-- Funcao principal que inicia a aplicacao
main :: IO ()
main = do
  maybeUsers <- carregaArquivo "./db/User.json"
  let initialState = AppState 0 (fromMaybe [] maybeUsers) "" MainMenu Nothing Nothing "" Nothing Nothing "" 0
  playIO
    (InWindow "HaskBankell - UFABC" (800, 600) (10, 10))
    (greyN 0.9)
    10
    initialState
    renderMenu
    handleEvent
    updateState

-- Funcao que renderiza o menu ou os estados correspondentes
renderMenu :: AppState -> IO Picture
renderMenu state = return $
  pictures
    [ case currentState state of
        MainMenu -> renderMainMenu
        AddUserID -> renderAddUserID (inputText state)
        AddUserName -> renderAddUserName (inputText state)
        AddUserSaldo -> renderAddUserSaldo (inputText state)
        RemoveUser -> renderRemoveUser (inputText state) (successMessage state)
        TransferSaldo -> renderTransferSaldo (inputText state)
        TransferSenderID -> renderTransferSenderID (inputText state)
        TransferValue -> renderTransferValue (inputText state)
        TransferReceiverID -> renderTransferReceiverID (inputText state)
        TransferParceladoSenderID -> renderTransferParceladoSenderID (inputText state)
        TransferParceladoValue -> renderTransferParceladoValue (inputText state)
        TransferParceladoReceiverID -> renderTransferParceladoReceiverID (inputText state)
        PagamentoParceladoID -> renderPagamentoParceladoID (inputText state)
        PagamentoParceladoValue -> renderPagamentoParceladoValue (inputText state)
        ExibirUsuarios -> renderExibirUsuarios (users state)
        SuccessMessageState -> renderSuccessMessage (successMessage state)
        ExitApp -> translate (-350) 100 . scale 0.2 0.2 . color black $ text "Saindo..."
        QuitState -> translate (-350) 100 . scale 0.2 0.2 . color black $ text "Saindo..."
    ]

-- Renderiza o menu principal
renderMainMenu :: Picture
renderMainMenu = pictures
  [ translate (-350) 200 . scale 0.3 0.3 . color black $ text "Escolha uma opcao:"
  , translate (-350) 150 . scale 0.3 0.3 . color black $ text "1 - Adicionar usuario"
  , translate (-350) 100 . scale 0.3 0.3 . color black $ text "2 - Exibir usuarios"
  , translate (-350) 50  . scale 0.3 0.3 . color black $ text "3 - Remover usuario"
  , translate (-350) 0   . scale 0.3 0.3 . color black $ text "4 - Transferir saldo"
  , translate (-350) (-50) . scale 0.3 0.3 . color black $ text "5 - Transferencia parcelada"
  , translate (-350) (-100) . scale 0.3 0.3 . color black $ text "6 - Pagamento parcelado"
  , translate (-350) (-150) . scale 0.3 0.3 . color black $ text "7 - Sair"
  , translate (-350) (-200) . scale 0.3 0.3 . color black $ text "-- HaskBankell - UFABC --"
  ]

renderAddUserID :: String -> Picture
renderAddUserID input = pictures
  [ translate (-350) 200 . scale 0.3 0.3 . color black $ text "Adicionando usuario"
  , translate (-350) 150 . scale 0.3 0.3 . color black $ text "Insira o ID do usuario: "
  , translate (-350) 50  . scale 0.3 0.3 . color black $ text input
  ]

renderAddUserName :: String -> Picture
renderAddUserName input = pictures
  [ translate (-350) 200 . scale 0.3 0.3 . color black $ text "Adicionando usuario"
  , translate (-350) 150 . scale 0.3 0.3 . color black $ text "Insira o nome do usuario: "
  , translate (-350) 50  . scale 0.3 0.3 . color black $ text input
  ]

renderAddUserSaldo :: String -> Picture
renderAddUserSaldo input = pictures
  [ translate (-350) 200 . scale 0.3 0.3 . color black $ text "Adicionando usuario"
  , translate (-350) 150 . scale 0.3 0.3 . color black $ text "Insira o saldo inicial: "
  , translate (-350) 50  . scale 0.3 0.3 . color black $ text input
  ]

renderRemoveUser :: String -> String -> Picture
renderRemoveUser input successMsg = pictures
  [ translate (-350) 200 . scale 0.3 0.3 . color black $ text "Removendo usuario"
  , translate (-350) 150 . scale 0.3 0.3 . color black $ text "Insira o ID do usuario: "
  , translate (-350) 50  . scale 0.3 0.3 . color black $ text input
  , translate (-350) (-50) . scale 0.3 0.3 . color black $ text successMsg
  ]

renderTransferSenderID :: String -> Picture
renderTransferSenderID input = pictures
  [ translate (-350) 200 . scale 0.3 0.3 . color black $ text "Transferencia de saldo"
  , translate (-350) 150 . scale 0.3 0.3 . color black $ text "Insira o ID do remetente: "
  , translate (-350) 50  . scale 0.3 0.3 . color black $ text input
  ]

renderTransferValue :: String -> Picture
renderTransferValue input = pictures
  [ translate (-350) 200 . scale 0.3 0.3 . color black $ text "Transferindo saldo"
  , translate (-350) 150 . scale 0.3 0.3 . color black $ text "Insira o valor da transferencia: "
  , translate (-350) 50  . scale 0.3 0.3 . color black $ text input
  ]

renderTransferReceiverID :: String -> Picture
renderTransferReceiverID input = pictures
  [ translate (-350) 200 . scale 0.3 0.3 . color black $ text "Transferencia de saldo"
  , translate (-350) 150 . scale 0.3 0.3 . color black $ text "Insira o ID do destinatario: "
  , translate (-350) 50  . scale 0.3 0.3 . color black $ text input
  ]

renderTransferParceladoSenderID :: String -> Picture
renderTransferParceladoSenderID input = pictures
  [ translate (-350) 200 . scale 0.3 0.3 . color black $ text "Transferencia Parcelada"
  , translate (-350) 150 . scale 0.3 0.3 . color black $ text "Insira o ID do remetente: "
  , translate (-350) 50  . scale 0.3 0.3 . color black $ text input
  ]

renderTransferParceladoValue :: String -> Picture
renderTransferParceladoValue input = pictures
  [ translate (-350) 200 . scale 0.3 0.3 . color black $ text "Insira o valor da transferencia"
  , translate (-350) 150 . scale 0.3 0.3 . color black $ text "parcelada: "
  , translate (-350) 50  . scale 0.3 0.3 . color black $ text input
  ]

renderTransferParceladoReceiverID :: String -> Picture
renderTransferParceladoReceiverID input = pictures
  [ translate (-350) 200 . scale 0.3 0.3 . color black $ text "Insira o ID do destinatario da"
  , translate (-350) 150 . scale 0.3 0.3 . color black $ text "transferencia parcelada: "
  , translate (-350) 50  . scale 0.3 0.3 . color black $ text input
  ]

renderPagamentoParceladoID :: String -> Picture
renderPagamentoParceladoID input = pictures
  [ translate (-350) 200 . scale 0.3 0.3 . color black $ text "Pagamento Parcelado"
  , translate (-350) 150 . scale 0.3 0.3 . color black $ text "Insira o ID do usuario: "
  , translate (-350) 50  . scale 0.3 0.3 . color black $ text input
  ]

renderPagamentoParceladoValue :: String -> Picture
renderPagamentoParceladoValue input = pictures
  [ translate (-350) 200 . scale 0.3 0.3 . color black $ text "Insira o valor a ser pago: "
  , translate (-350) 50  . scale 0.3 0.3 . color black $ text input
  ]

renderTransferSaldo :: String -> Picture
renderTransferSaldo input = pictures
  [ translate (-350) 200 . scale 0.3 0.3 . color black $ text "Transferindo saldo"
  , translate (-350) 150 . scale 0.3 0.3 . color black $ text "Insira os detalhes: "
  , translate (-350) 50  . scale 0.3 0.3 . color black $ text input
  ]

-- Funcao para exibir usuarios de forma compacta e clara
renderExibirUsuarios :: [User] -> Picture
renderExibirUsuarios users = pictures $
  [ translate (-350) 200 . scale 0.3 0.3 . color black $ text "Lista de usuarios:"]
  ++ zipWith (\u n -> translate (-350) (150 - n * 40) . scale 0.3 0.3 . color black $ text (formatUser u)) users [1..]

-- Exibe a mensagem de sucesso por alguns segundos
renderSuccessMessage :: String -> Picture
renderSuccessMessage msg = translate (-350) 0 . scale 0.3 0.3 . color black $ text (replaceInMessage msg)

-- Função para substituir mensagens e adicionar quebras de linha
replaceInMessage :: String -> String
replaceInMessage "Erro: Usuario no possui saldo devedor em sua conta" = "Erro: Usuario nao possui saldo devedor em\nsua conta"
replaceInMessage other = other

-- Formata cada usuario de maneira mais compacta
formatUser :: User -> String
formatUser (User uid nome saldo _ _) = "ID: " ++ show uid ++ " | Nome: " ++ nome ++ " | Saldo: " ++ show saldo

-- Funcao que lida com eventos, incluindo cliques do mouse
handleEvent :: Event -> AppState -> IO AppState
-- O clique do mouse so funciona quando no estado MainMenu
handleEvent (EventKey (MouseButton LeftButton) Down _ (x, y)) state
  | currentState state == MainMenu = handleMouseClick x y state
  | otherwise = return state
handleEvent (EventKey (Char c) Down _ _) state
  | currentState state `elem` [AddUserID, AddUserName, AddUserSaldo, RemoveUser, TransferSenderID, TransferValue, TransferReceiverID, TransferParceladoSenderID, TransferParceladoValue, TransferParceladoReceiverID, PagamentoParceladoID, PagamentoParceladoValue]
  = return state { inputText = inputText state ++ [c] }
handleEvent (EventKey (SpecialKey KeyBackspace) Down _ _) state =
  return state { inputText = if null (inputText state) then "" else init (inputText state) }
handleEvent (EventKey (SpecialKey KeyEnter) Down _ _) state = handleEnterPress state
handleEvent _ state = return state

-- Funcao que lida com cliques do mouse
handleMouseClick :: Float -> Float -> AppState -> IO AppState
handleMouseClick x y state
  | y > 150 && y < 200 && x > -350 && x < 350 = return state { currentState = AddUserID }
  | y > 100 && y < 150 && x > -350 && x < 350 = return state { currentState = ExibirUsuarios }
  | y > 50 && y < 100 && x > -350 && x < 350 = return state { currentState = RemoveUser }
  | y > 0 && y < 50 && x > -350 && x < 350 = return state { currentState = TransferSenderID }
  | y > -50 && y < 0 && x > -350 && x < 350 = return state { currentState = TransferParceladoSenderID }
  | y > -100 && y < -50 && x > -350 && x < 350 = return state { currentState = PagamentoParceladoID }
  | y > -150 && y < -100 && x > -350 && x < 350 = return state { currentState = QuitState, delayTime = 5 }
  | otherwise = return state

-- Lida com a logica de pressionar Enter para cada estado
handleEnterPress :: AppState -> IO AppState
handleEnterPress state =
  case currentState state of
    AddUserID -> do
      case readMaybe (inputText state) :: Maybe Integer of
        Nothing -> return state { inputText = "Erro: ID invalido", successMessage = "Erro: ID invalido", delayTime = 3, currentState = SuccessMessageState }
        Just userId -> 
          if any (\u -> getUserId u == userId) (users state)
            then return state { inputText = "Erro: ID ja esta em uso", successMessage = "Erro: ID ja esta em uso", delayTime = 3, currentState = SuccessMessageState }
            else return state { tempUserId = Just userId, inputText = "", currentState = AddUserName }

    AddUserName -> return state { tempUserName = inputText state, inputText = "", currentState = AddUserSaldo }

    AddUserSaldo -> do
      case readMaybe (inputText state) :: Maybe Float of
        Nothing -> return state { inputText = "Erro: Saldo invalido", currentState = SuccessMessageState, successMessage = "Erro: Saldo invalido", delayTime = 3 }
        Just saldo -> do
          let newUser = User (fromMaybe 0 (tempUserId state)) (tempUserName state) saldo 0 "senha"
          let updatedUsers = addUsuario newUser (users state)
          salvaUsuario "./db/User.json" updatedUsers
          return state { users = updatedUsers, inputText = "", currentState = MainMenu }

    RemoveUser -> do
      case readMaybe (inputText state) :: Maybe Integer of
        Nothing -> return state { inputText = "Erro: ID invalido", successMessage = "Erro: ID invalido", delayTime = 3, currentState = SuccessMessageState }
        Just userId -> do
          let updatedUsers = removeUsuario userId "senha" (users state)
          case updatedUsers of
            Nothing -> return state { inputText = "Erro: Usuario nao encontrado", currentState = SuccessMessageState, successMessage = "Erro: Usuario nao encontrado", delayTime = 3 }
            Just newUsers -> do
              salvaUsuario "./db/User.json" newUsers
              return state { users = newUsers, inputText = "Usuario removido com sucesso", currentState = SuccessMessageState, successMessage = "Usuario removido com sucesso", delayTime = 3 }

    TransferSenderID -> do
      case readMaybe (inputText state) :: Maybe Integer of
        Nothing -> return state { inputText = "Erro: ID invalido", currentState = SuccessMessageState, successMessage = "Erro: ID invalido", delayTime = 3 }
        Just senderId -> do
          let userExists = any (\u -> getUserId u == senderId) (users state)
          if not userExists
            then return state { inputText = "Erro: ID invalido", currentState = SuccessMessageState, successMessage = "Erro: ID invalido", delayTime = 3 }
            else return state { tempUserId = Just senderId, inputText = "", currentState = TransferValue }

    TransferValue -> do
      case readMaybe (inputText state) :: Maybe Float of
        Nothing -> return state { inputText = "Erro: Valor nao e um numero", currentState = SuccessMessageState, successMessage = "Erro: Valor nao e um numero", delayTime = 3 }
        Just value -> return state { transferValue = Just value, inputText = "", currentState = TransferReceiverID }

    TransferReceiverID -> do
      case readMaybe (inputText state) :: Maybe Integer of
        Nothing -> return state { inputText = "Erro: ID invalido", currentState = SuccessMessageState, successMessage = "Erro: ID invalido", delayTime = 3 }
        Just receiverId -> do
          let userExists = any (\u -> getUserId u == receiverId) (users state)
          if not userExists
            then return state { inputText = "Erro: ID invalido", currentState = SuccessMessageState, successMessage = "Erro: ID invalido", delayTime = 3 }
            else do
              let updatedUsers = transferirSaldo (fromMaybe 0 (tempUserId state)) "senha" receiverId (fromMaybe 0 (transferValue state)) (users state)
              case updatedUsers of
                Left errMsg -> return state { inputText = errMsg, currentState = SuccessMessageState, successMessage = errMsg, delayTime = 3 }
                Right users -> do
                  salvaUsuario "./db/User.json" users
                  return state { users = users, inputText = "Transferencia realizada com sucesso", currentState = SuccessMessageState, successMessage = "Transferencia realizada com sucesso", delayTime = 3 }

    TransferParceladoSenderID -> do
      case readMaybe (inputText state) :: Maybe Integer of
        Nothing -> return state { inputText = "Erro: ID invalido", currentState = SuccessMessageState, successMessage = "Erro: ID invalido", delayTime = 3 }
        Just senderId -> do
          let userExists = any (\u -> getUserId u == senderId) (users state)
          if not userExists
            then return state { inputText = "Erro: ID invalido", currentState = SuccessMessageState, successMessage = "Erro: ID invalido", delayTime = 3 }
            else return state { tempUserId = Just senderId, inputText = "", currentState = TransferParceladoValue }

    TransferParceladoValue -> do
      case readMaybe (inputText state) :: Maybe Float of
        Nothing -> return state { inputText = "Erro: Valor nao e um numero", currentState = SuccessMessageState, successMessage = "Erro: Valor nao e um numero", delayTime = 3 }
        Just value -> return state { transferValue = Just value, inputText = "", currentState = TransferParceladoReceiverID }

    TransferParceladoReceiverID -> do
      case readMaybe (inputText state) :: Maybe Integer of
        Nothing -> return state { inputText = "Erro: ID invalido", currentState = SuccessMessageState, successMessage = "Erro: ID invalido", delayTime = 3 }
        Just receiverId -> do
          let userExists = any (\u -> getUserId u == receiverId) (users state)
          if not userExists
            then return state { inputText = "Erro: ID invalido", currentState = SuccessMessageState, successMessage = "Erro: ID invalido", delayTime = 3 }
            else do
              let updatedUsers = transferirParcelado (fromMaybe 0 (tempUserId state)) "senha" receiverId (fromMaybe 0 (transferValue state)) (users state)
              case updatedUsers of
                Left errMsg -> return state { inputText = errMsg, currentState = SuccessMessageState, successMessage = errMsg, delayTime = 3 }
                Right users -> do
                  salvaUsuario "./db/User.json" users
                  return state { users = users, inputText = "Transferencia parcelada realizada com sucesso", currentState = SuccessMessageState, successMessage = "Transferencia parcelada realizada com sucesso", delayTime = 3 }

    PagamentoParceladoID -> do
      case readMaybe (inputText state) :: Maybe Integer of
        Nothing -> return state { inputText = "Erro: ID invalido", currentState = SuccessMessageState, successMessage = "Erro: ID invalido", delayTime = 3 }
        Just userId -> do
          let userExists = any (\u -> getUserId u == userId) (users state)
          if not userExists
            then return state { inputText = "Erro: ID invalido", currentState = SuccessMessageState, successMessage = "Erro: ID invalido", delayTime = 3 }
            else return state { tempUserId = Just userId, inputText = "", currentState = PagamentoParceladoValue }

    PagamentoParceladoValue -> do
      case readMaybe (inputText state) :: Maybe Float of
        Nothing -> return state { inputText = "Erro: Valor nao e um numero", currentState = SuccessMessageState, successMessage = "Erro: Valor nao e um numero", delayTime = 3 }
        Just value -> do
          let updatedUsers = pagamentoParcelado (fromMaybe 0 (tempUserId state)) "senha" value (users state)
          case updatedUsers of
            Left errMsg -> return state { inputText = errMsg, currentState = SuccessMessageState, successMessage = errMsg, delayTime = 3 }
            Right users -> do
              salvaUsuario "./db/User.json" users
              return state { users = users, inputText = "Pagamento parcelado realizado com sucesso", currentState = SuccessMessageState, successMessage = "Pagamento parcelado realizado com sucesso", delayTime = 3 }

    ExibirUsuarios -> return state { currentState = MainMenu }

    _ -> return state

-- Funcao de atualizacao do estado para lidar com o delay e saida do programa
updateState :: Float -> AppState -> IO AppState
updateState dt state
  | currentState state == SuccessMessageState && delayTime state > 0 =
      return state { delayTime = delayTime state - dt }
  | currentState state == SuccessMessageState && delayTime state <= 0 =
      return state { currentState = MainMenu, successMessage = "", inputText = "" }
  | currentState state == QuitState && delayTime state > 0 =
      return state { delayTime = delayTime state - dt }
  | currentState state == QuitState && delayTime state <= 0 = exitSuccess
  | otherwise = return state

exitApp :: IO AppState
exitApp = do
  putStrLn "Saindo..."
  return $ AppState 0 [] "" QuitState Nothing Nothing "" Nothing Nothing "" 5
