import System.Random
import Control.Monad

main :: IO ()
main = gameLoop 1 100

gameLoop :: Int -> Int -> IO ()
gameLoop a b = do
    if a > b then do
        putStrLn "Похоже, вы меня обманываете... Давайте еще раз?"
        main
    else do
        u <- randomRIO (a, b)
        putStrLn $ "Ваше число - " ++ show u ++ "?"
        userInput <- getLine
        case userInput of
            "=" -> putStrLn "Ура, я так и знал!"
            ">" -> gameLoop (u + 1) b
            "<" -> gameLoop a (u - 1)
            _    -> do
                putStrLn "Что-то не сходится... Попробуйте еще раз!"
                gameLoop a b