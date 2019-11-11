
newtype Env = Env [Map.Map Ident Value]

pushEnv :: Env -> Env
pushEnv (Env envs) = Map.empty : envs

popEnv :: Env -> Env
popEnv (Env (_:envs)) = envs
popEnv (Env []) = undefined

defineVar :: Ident -> Value -> Env -> Env
defineVar n v (Env (env:_)) = Map.insert n v env
defineVar _ _ (Env []) = undefined

getVar :: Ident -> Env -> Maybe Value
getVar n (Env (env:envs)) = case Map.lookup n env of
    Just v -> Just v
    Nothing -> getVar n envs
getVar _ (Env []) = Nothing

setVar :: Ident -> Value -> Env -> Maybe Env
setVar n v (Env (env:envs)) = case Map.lookup n env of
    Just v -> Just $ Map.insert n v env
    Nothing -> Just $ setVar n v envs
setVar _ _ (Env []) = Nothing
