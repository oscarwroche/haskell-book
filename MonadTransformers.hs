{-# LANGUAGE OverloadedStrings #-}

import Data.Text

-- Imports that will be needed later:
import qualified Data.Text.IO as T
import Data.Map as Map
import Control.Applicative


data LoginError = InvalidEmail
                | NoSuchUser
                | WrongPassword
                deriving Show

getDomain :: Text -> Either LoginError Text
getDomain email =
  case splitOn "@" email of
    [name, domain] -> Right domain
    _              -> Left InvalidEmail

printResult :: Either LoginError Text -> IO ()
printResult = T.putStrLn . either
  (const "ERROR: Invalid domain")
  (append "Domain: ")

printResult' :: Either LoginError Text -> IO ()
printResult' domain =
  case domain of
    Right text -> T.putStrLn (append "Domain: " text)
    Left InvalidEmail -> T.putStrLn "ERROR: Invalid domain"

getToken :: EitherIO LoginError Text
getToken = do
  liftIO (T.putStrLn "Enter email address: ")
  input <- liftIO T.getLine
  liftEither (getDomain input)

users :: Map Text Text
users = Map.fromList [("example.com", "abcde1"), ("localhost", "password")]

userLogin :: IO (Either LoginError Text)
userLogin = do
  token <- getToken

  case token of
    Right domain ->
      case Map.lookup domain users of
        Just userpw -> do
          T.putStrLn "Enter password:"
          password <- T.getLine

          if userpw == password
             then return token

             else return (Left WrongPassword)
        Nothing -> return (Left NoSuchUser)
    left -> return left

data EitherIO e a = EitherIO {
    runEitherIO :: IO (Either e a)
}

instance Functor (EitherIO e) where
  fmap f = EitherIO . fmap (fmap f) . runEitherIO

instance Applicative (EitherIO e) where
  pure    = EitherIO . return . Right
  f <*> x = EitherIO $ liftA2 (<*>) (runEitherIO f) (runEitherIO x)

instance Monad (EitherIO e) where
  return  = pure
  x >>= f = EitherIO $ runEitherIO x >>= either (return . Left) (runEitherIO . f)

liftEither :: Either e a -> EitherIO e a
liftEither x = EitherIO (return x)

liftIO :: IO a -> EitherIO e a
liftIO x = EitherIO (fmap Right x)
