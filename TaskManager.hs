--Author: Artur Francisco Pereira Carvalho
--Matéria: Paradigmas de Programação

module GerenciadorTarefas where
-- Importação do exitSuccess para sair do programa
import System.Exit (exitSuccess)
-- Definição do tipo de dados Tarefa
data Tarefa = Tarefa { descricao :: String } deriving (Show)

-- Função para adicionar uma nova tarefa à lista
adicionarTarefa :: [Tarefa] -> IO [Tarefa]
adicionarTarefa tarefas = do
  putStrLn "Digite a descrição da nova tarefa:"
  descricao <- getLine
  let novaTarefa = Tarefa { descricao = descricao }
  return (tarefas ++ [novaTarefa])

-- Função para remover uma tarefa da lista
removerTarefa :: [Tarefa] -> IO [Tarefa]
removerTarefa tarefas = do
  putStrLn "Digite o número da tarefa que você deseja remover:"
  listarTarefas tarefas
  numero <- readLn
  let indice = numero - 1
  if indice >= 0 && indice < length tarefas
    then do
      let (antes, depois) = splitAt indice tarefas
      return (antes ++ tail depois)
    else do
      putStrLn "Número de tarefa inválido."
      return tarefas

-- Função para exibir a lista de tarefas
listarTarefas :: [Tarefa] -> IO ()
listarTarefas tarefas = do
  putStrLn "Lista de tarefas:"
  putStrLn "-----------------"
  if null tarefas
    then putStrLn "Nenhuma tarefa."
    else mapM_ (\(i, tarefa) -> putStrLn (show i ++ ". " ++ descricao tarefa)) (zip [1..] tarefas)

-- Função principal que exibe o menu e trata as operações
main :: IO ()
main = do
  putStrLn "Bem-vindo ao Gerenciador de Tarefas!"
  menu []

-- Função auxiliar para exibir o menu e aguardar a entrada do usuário
menu :: [Tarefa] -> IO ()
menu tarefas = do
  putStrLn "\nOpções:"
  putStrLn "1. Adicionar tarefa"
  putStrLn "2. Remover tarefa"
  putStrLn "3. Exibir lista de tarefas"
  putStrLn "4. Sair"

  opcao <- getLine
  case opcao of
    "1" -> do
      novasTarefas <- adicionarTarefa tarefas
      menu novasTarefas
    "2" -> do
      novasTarefas <- removerTarefa tarefas
      menu novasTarefas
    "3" -> do
      listarTarefas tarefas
      menu tarefas
    "4" -> exitSuccess
    _   -> do
      putStrLn "Opção inválida. Tente novamente."
      menu tarefas
