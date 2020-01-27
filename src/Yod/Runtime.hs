module Yod.Runtime where

import           Control.Monad.Except
import           Control.Monad.Reader
import qualified Data.Map             as Dict
import qualified Data.Text            as T
import           Yod.Expr
import           Yod.Type             (Type)
import qualified Yod.Type             as Type

type Runtime = ExceptT Type.Error (Reader Env)

withVar :: (T.Text, Expr) -> Runtime a -> Runtime a
withVar (name, value) = local $ Dict.insert name value

with :: Env -> Runtime a -> Runtime a
with = local . Dict.union

lookupVar :: T.Text -> Runtime Value
lookupVar name = do
    env <- ask
    case Dict.lookup name env of
        Just expr -> eval expr
        Nothing   -> throwError $ Type.UnknownIdentifier name

typeOf :: Value -> Runtime Type
typeOf = \case
    Int _ -> pure Type.Int
    Bool _ -> pure Type.Bool
    Char _ -> pure Type.Char
    Float _ -> pure Type.Float
    String _ -> pure Type.String
    Closure (argName, expectedArgType) body closureEnv -> do
        bodyValue <- eval body
        argValue  <- lookupVar argName
        argType   <- typeOf argValue

        if expectedArgType == argType
        then do
            bodyType <- typeOf bodyValue
            with closureEnv $ pure $ Type.Lambda bodyType argType
        else throwError $ Type.Missmatch expectedArgType argType

eval :: Expr -> Runtime Value
eval = \case
    Value value -> pure value

    Lambda var body -> Closure var body <$> ask

    Var name -> lookupVar name

    Apply lambda arg -> do
        closure  <- eval lambda
        argValue <- eval arg

        case closure of
            Closure (argName, expectedArgType) body closureEnv ->
                with closureEnv $ withVar (argName, Value argValue) $ do
                    argType <- typeOf argValue
                    if expectedArgType == argType
                    then eval body
                    else throwError $ Type.Missmatch expectedArgType argType
            value -> throwError $ Type.NotAFunction $ T.pack $ show value

    LetIn letEnv body -> with letEnv $ eval body

    If conditionExpr thenExpr elseExpr -> do
        condition <- eval conditionExpr
        thenValue <- eval thenExpr
        elseValue <- eval elseExpr

        case condition of
            Bool isSatisfied -> do
                thenType <- typeOf thenValue
                elseType <- typeOf elseValue

                if thenType == elseType
                then pure $ if isSatisfied then thenValue else elseValue
                else throwError $ Type.Missmatch thenType elseType

            value -> throwError . Type.Missmatch Type.Bool =<< typeOf value

runtimeIn :: Env -> Expr -> Either Type.Error Value
runtimeIn env expr = runReader (runExceptT $ eval expr) env

runtime :: Expr -> Either Type.Error Value
runtime = runtimeIn mempty
