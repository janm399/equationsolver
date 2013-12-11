{-# OPTIONS -XOverloadedStrings #-}
module Main where
  import EquationSolver.Lexer
  import EquationSolver.Parser
  import Network.AMQP
  import qualified Data.ByteString.Lazy.Char8 as BL

  main :: IO ()
  main = do
    conn <- openConnection "localhost" "/" "guest" "guest"
    chan <- openChannel conn

    consumeMsgs chan "equationsolver" Ack solve

    getLine

    closeConnection conn
    putStrLn "Done"

    where
      solve :: (Message, Envelope) -> IO ()
      solve (msg, env) = do
        let x = runParse $ tokenize (BL.unpack $ msgBody msg)
        putStrLn $ show x
        ackEnv env