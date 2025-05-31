module Parser (parse) where 
import Data.Set as Set
import Lexer (Token(..), StreamToken(..), Content(..))
import qualified Data.Map.Strict as Dm
import Data.Maybe (catMaybes)
import Ast 
import Control.Monad.Writer 
import Data.Bifunctor (Bifunctor(bimap))
import Data.Text (pack, splitOn, Text, takeWhile, head, unpack)
import Control.Exception (throw)
import Exceptions 


type Rule = [Atom]

data Atom = T Token | V String 
    deriving (Ord, Show)

instance Eq Atom where 
    (T a) == (T b) = a == b 
    (V a) == (V b) = a == b
    _ == _ = False


type Grammar = Dm.Map String [Rule]

startRule :: String 
startRule = "Ast"

startPosition :: Position 
startPosition = Position {prod="", rule=[V "Ast"], pos=0, followSet= singleton EOF}



isToken :: Atom -> Bool 
isToken (T _) = True 
isToken (V _) = False 

(!?) :: [a] -> Int -> Maybe a
(!?) xs n
    | length xs <= n = Nothing 
    | otherwise = Just (xs !! n)
-------------------- Pretty --------------------
class Pretty a where 
    pretty :: a -> String

prettyAuto :: Automaton -> String 
prettyAuto = fst . Dm.foldlWithKey (\(acc, i) k v -> (acc ++ "State " ++ show i ++ "\n"++ prettyState 0 k ++ "with transitions\n" ++ prettyAA v, i+1)) ("Automaton:\n", 0)

prettyAA :: Dm.Map Atom State -> String 
prettyAA  = Dm.foldlWithKey (\acc k v -> acc ++ pretty k ++ " =>\n "++ prettyState 1 v) "" 

prettyState :: Int -> State -> String
prettyState i s = let indent = replicate (4*i) ' ' in
    Set.foldl (\acc p -> acc ++ indent ++ pretty p ++ "\n" ) "" s

instance Pretty Position where 
    pretty p = 
        let lookahead = reverse . (\(_:xs) -> '}':xs) . reverse $ Set.foldl (\acc' e -> acc' ++ pretty (T e) ++ ",") "{" $ followSet p in
        let pRule = prettyPosRule 0 (pos p) $ rule p in
        prod p ++ " -> " ++ pRule ++ " <=> " ++ lookahead  
        where 
        prettyPosRule :: Int -> Int -> Rule -> String
        prettyPosRule iter po (a:as) 
            | iter == po = " . " ++ pretty a ++ prettyPosRule (iter + 1) po as
            | iter + 1 == po && length (a:as) == 1 = " " ++ pretty a ++ " ."
            | otherwise = " " ++ pretty a ++ prettyPosRule (iter + 1) po as 
        prettyPosRule _ _ _ = ""

instance Pretty Atom where 
    pretty (T t) = show t
    pretty (V v) = v
-------------------- GOTO --------------------
type Automaton = Dm.Map State (Dm.Map Atom State )

--  findTransitionTargets . mergeClosures . initialClosure test $ Position {prod = "Stm", rule = [V "Stm"], pos = 0, followSet = fromList [EOF]}

--   prettyAuto . transition test . mergeClosures . initialClosure test $ Position {prod = "", rule = [V "Stm"], pos = 0, followSet = fromList [EOF]}

--  findTransitionTargets . mergeClosures . closure test $ fromList [Position {prod = "Term", rule = [T LeftParen, V "Exp", T RightParen], pos = 1, followSet = fromList [RightParen, Plus, Minus]}]



--  closure test $ fromList [Position {prod = "Exp", rule = [V "Exp",V "Op",V "Term"], pos = 1, followSet = fromList [RightParen,Plus,Minus]},Position {prod = "Term", rule = [T LeftParen,V "Exp",T RightParen], pos = 2, followSet = fromList [RightParen,Plus,Minus]}]



findTransitionTargets :: State -> Dm.Map Atom (Set Position)
findTransitionTargets state = 
    let targets = Set.fromList . catMaybes  $ Set.foldl (\acc p -> 
                    let target =  rule p !? pos p in
                    target:acc
                ) [] state
    in 
    Set.foldl (\acc t -> 
                let b = Dm.singleton t $ Set.foldl (\acc2 p -> 
                           let target = rule p !? pos p in
                           if target == Just t then
                           Set.insert (incrementPos p) acc2
                           else acc2
                        ) Set.empty state    
                in
                Dm.unionWith union acc b
            ) Dm.empty targets

transitionClosure :: Grammar -> Automaton -> Automaton
transitionClosure g a = 
    let res = Prelude.foldl (\acc (_, mas) ->  
                let (_, states) = unzip $ Dm.toList mas in
                let newA = Prelude.foldl(\acc' s -> acc' `Dm.union` transition g s) Dm.empty states
                in
                acc `Dm.union` newA 
            ) a $ Dm.toList a 
    in
    if res == a then 
        res 
    else transitionClosure g res

transition :: Grammar -> State -> Automaton 
transition g state =
    let targets = findTransitionTargets state in 
    let a = Prelude.foldl (\acc (a', set) -> 
                                let ns = closure g set in
                                let newM = Dm.singleton a' ns in 
                                Dm.union acc newM
                            ) Dm.empty $ Dm.toList targets
    in
    Dm.singleton state a 

findLoci :: State -> Set Atom 
findLoci s =   Set.fromList . catMaybes  $ Set.foldl (\acc p -> 
                    let target =  rule p !? pos p in
                    target:acc
                ) [] s

-------------------- Closure --------------------
data Position = Position {prod::String, rule::Rule, pos::Int, followSet :: Set Token}
    deriving (Ord, Show)
instance Eq Position where 
    a == b = 
        (prod a == prod b) && (rule a == rule b) && (pos a == pos b)

type State = Set Position 


incrementPos :: Position -> Position 
incrementPos p = Position {prod=prod p, rule=rule p, pos= pos p + 1, followSet=followSet p} 


-- >>>  mergeClosures $ initialClosure simpleGrammar $ Position {prod = "S", rule = [V "S"], pos = 0, followSet = fromList [EOF]}
-- fromList [Position {prod = "S", rule = [V "S"], pos = 0, followSet = fromList [EOF]},Position {prod = "S", rule = [V "S",V "A"], pos = 0, followSet = fromList [EOF,Ident]},Position {prod = "S", rule = [V "S",V "B"], pos = 0, followSet = fromList [EOF,Ident]}]

mergeClosures :: State -> State 
mergeClosures = helper . toList  where 
    helper :: [Position] -> State 
    helper [] = Set.empty 
    helper (x:y:rest) 
        | x == y =
            let s = Position {prod=prod x, rule=rule x, pos=pos x, followSet= followSet x `union` followSet y} in
            helper (s:rest) 
        | otherwise = Set.singleton x `union` helper (y:rest)
    helper [x] = Set.singleton x

fSet :: Grammar -> Atom -> Set Token
fSet _ (T t)  = Set.singleton t 
fSet g (V v) = findFromVar g v

initialClosure :: Grammar -> Position -> State
initialClosure g p = closure g initial where 
    initial = closureR g p

closure :: Grammar -> State -> State 
closure g state = 
        let newState = Set.foldl (\acc po -> 
                    let cl = closureR g po in
                    acc `union` cl
                ) Set.empty state 
        in
        if newState == state then 
            mergeClosures newState 
        else 
            closure g newState


-- >>> closureR test $ Position {prod = "Term", rule = [T LeftParen,V "Exp",T RightParen], pos = 2, followSet = fromList [RightParen,Plus,Minus]}


closureR:: Grammar -> Position -> State
closureR g p 
    | length (rule p) == pos p = Set.singleton p 
    | otherwise = 
        let locus = rule p !! pos p in
        case locus of 
            T _ -> Set.singleton p
            V var -> 
                let tlup = g Dm.!? var in
                case tlup of
                Nothing -> error $ "Non-Term " ++ show var ++ " has no rules (maybe you misspelled?)"
                Just lup -> 
                    let fset = 
                            if length (rule p) > (pos p + 1) then
                                let next = rule p !! (pos p + 1) in
                                fSet g next 
                            else 
                                followSet p
                    in
                    Prelude.foldl(\acc r -> 
                        let pp = Position {prod=var, rule=r, pos=0, followSet=fset} in
                        Set.insert pp acc
                    ) (Set.singleton p) lup

generatePositions :: String -> [Rule] ->  Set Position
generatePositions  s = 
    Prelude.foldl (\acc r -> 
                acc `Set.union` fromList [Position {prod=s,rule=r, pos=x, followSet=Set.empty} | x <- [0..(length r)]]
            ) Set.empty  

-------------------- FOLLOW --------------------

followTable :: Grammar -> Dm.Map String (Set Token)
followTable g = 
    let findT = findTable g in
    let mapl = Dm.toList g in 
    let initial = Prelude.foldl (\acc (s, rule) -> acc <> foo s rule g) emptyFollowA mapl 
    in
    let tokensFromFirst = 
            Prelude.foldl (\acc (x, ys) ->
                let m = Set.foldl (\acc2 y -> acc2 `union` (findT Dm.! y)) Set.empty ys
                in
                let nm = Dm.singleton x m in 
                Dm.unionWith Set.union nm acc
            ) Dm.empty . Dm.toList $ firsts initial
    in
    let temp = FollowAction {terminals=tokensFromFirst, firsts=Dm.empty, follows=Dm.empty} in
    let aaa = initial <> temp in 
    terminals $ aaa <> repeated aaa


repeated :: FollowAction -> FollowAction    
repeated acc' = 
        let followT = terminals acc' in
        let fi = follows acc' in
        let fol = Prelude.foldl (\acc (x, ys) -> 
                    let m = Set.foldl (\acc2 y -> acc2 `union` (followT Dm.! y)) Set.empty ys
                    in 
                    let nm = Dm.singleton x m in 
                    Dm.unionWith Set.union nm acc
                ) followT $ Dm.toList fi 
        in
        let next = FollowAction {terminals=fol, firsts=Dm.empty, follows=fi} in
        if acc' == next then 
            next 
        else repeated next


foo ::  String -> [Rule] -> Grammar -> FollowAction
foo s [] _ = emptyFollowA
foo s rs g = Prelude.foldl (\acc r -> acc <> analyze s r) emptyFollowA rs  

data FollowAction = FollowAction {terminals :: Dm.Map String (Set Token), firsts :: Dm.Map String (Set String), follows :: Dm.Map String (Set String)}
    deriving (Show)

emptyFollowA :: FollowAction 
emptyFollowA = FollowAction {terminals=Dm.empty, firsts=Dm.empty, follows=Dm.empty}

instance Eq FollowAction where 
    a == b = terminals a == terminals b

instance Semigroup FollowAction where 
    FollowAction {terminals=at, firsts=afi, follows=afo} 
        <> FollowAction {terminals=bt, firsts=bfi, follows=bfo} = 
                FollowAction {terminals= Dm.unionWith Set.union at bt, firsts= Dm.unionWith Set.union afi bfi, follows= Dm.unionWith Set.union afo bfo}

analyze :: String -> Rule -> FollowAction
analyze s [V va] 
    | s == startRule = FollowAction {terminals=Dm.singleton s (Set.singleton EOF), firsts=Dm.empty, follows=Dm.singleton va (Set.singleton s)}
    | otherwise = FollowAction {terminals=Dm.empty, firsts=Dm.empty, follows=Dm.singleton va (Set.singleton s)}
analyze s ((V va):(V vb):rest) =
        let newM = Dm.singleton va (Set.singleton vb) in 
        FollowAction {terminals= Dm.empty, firsts=newM, follows=Dm.empty} <> analyze s (V vb:rest)
analyze s ((V va):(T t):rest) =
    let newM = Dm.singleton va (Set.singleton t) in 
    FollowAction {terminals=newM, firsts=Dm.empty, follows=Dm.empty} <> analyze s rest
analyze s (_:rest) = analyze s rest
analyze _ _ = emptyFollowA

-------------------- FIND -------------------- 
findTable :: Grammar -> Dm.Map String (Set Token) 
findTable g = Dm.foldlWithKey (\acc k v ->   Dm.insert k (find k v g) acc) Dm.empty g  

findFromVar :: Grammar -> String -> Set Token 
findFromVar g s = 
    let l = g Dm.!? s in 
    case l of 
    Just l' -> find s l' g
    Nothing -> error "Dm.! in findFromVar"

find :: String -> [Rule] -> Grammar -> Set Token
find s rs g = 
    let (tt, var) = Prelude.foldl (\(sacc, var) (r:_) -> case r of
                                        T t -> (Set.insert t sacc, var)
                                        V str -> (sacc, str)
                  ) (Set.empty, "") $ Prelude.filter (\r@(a:_) -> length r == 1 || isToken a) rs
    in
    if tt == Set.empty then
        let lup = Dm.lookup var g in
        let next = Set.empty `maybe` (\v -> find var v g)  in
        next lup
    else 
        tt
-------------------- Actions --------------------
data Action = Accept | Shift | Reduce
    deriving (Show)

parse :: Bool -> [StreamToken] -> IO Ast 
parse v tokens = do 
    g <- readFile "grammar.txt"
    let grammar = parseGrammar g 
    -- print grammar
    let initialState = initialClosure grammar startPosition 
    let t =  transitionClosure grammar . transition grammar . mergeClosures $ initialState 
    -- print $ length t
    let (actions, log)  = runWriter $ action grammar [initialState] [] tokens 
    -- Need to implement a verbose flag
    _ <- if v then Prelude.foldr ((>>) . putStrLn) (putStrLn "") log
            else return ()
    return $ lastParse actions



action :: Grammar -> [State] -> ParseStack ->  [StreamToken] -> Writer [String] ParseStack 
action g (state:stateStack) ps (st:tokens) =  do
    tell ["stack snapshot: " ++ show ps]
    let (nextAction, p) = findAction state st  
    case nextAction of 
        Shift -> do
            tell ["in shift"]
            let possibleTs = transition g state 
            let nextStatet = (let pts = possibleTs Dm.!? state in 
                                case pts of 
                                Nothing -> error "no possible state"
                                Just res -> res) Dm.!? T (token st)  
            case nextStatet of 
                Nothing -> error "in shift cant find next state"
                Just nexState ->
                    action g (nexState:state:stateStack) (Pt st:ps) tokens
        Reduce -> do
            tell ["in reduce"]
            let ((newStack, toPop), _) = runWriter $ ruleFuncs (rule p) ps  
            let newStateStack = Prelude.drop toPop (state:stateStack)
            let possibleTs = transition g $ Prelude.head newStateStack 
            let nextStatet = (let pts = possibleTs Dm.!? Prelude.head newStateStack in 
                                case pts of 
                                Nothing -> error "no possible state"
                                Just res -> res) Dm.!? V (prod p)  
            case nextStatet of 
                Nothing -> error "in reduce cant find next state, probably cause by misaligned stack"
                Just nextState ->
                    action g (nextState:newStateStack) newStack (st:tokens)
        Accept -> do
            let ((newStack, _), _) = runWriter $ ruleFuncs (rule p) ps  
            tell ["accepted"]
            return newStack
action _ _ _ _ = error "something went wrong bruhh"
    

canBeReduced :: Position -> Token -> Bool 
canBeReduced p st = st `elem` followSet p

findAction :: State ->  StreamToken -> (Action, Position) 
findAction state st = 
    if isAccepting state (token st) then (Accept, startPosition) else 
    let (s, r) = bimap catMaybes  catMaybes $ Set.foldl (\(acc, acc2) p -> 
                            if length (rule p) == pos p && canBeReduced p (token st) then  
                                (acc, Just p:acc2)
                            else 
                                let res = rule p !? pos p >>= 
                                        (\t ->
                                            if t == T (token st) then 
                                                Just p
                                            else 
                                                Nothing
                                        )
                                in
                                (res:acc, acc2)
                        ) ([], []) state
    in
    case (s, r) of 
        (p:_, []) -> (Shift, p)
        ([],  p:_) -> (Reduce, p)
        (p:_, p2:_) -> throw $ ShiftReduce $ "Shift/Reduce at: " ++ show (loc st) ++ " in\n" ++ prettyState 0 state ++ "\n" ++ pretty p ++ "\n" ++ pretty p2 ++ "\non stack: " ++ show (token st)
        (_ , _) -> throw $ FindAction $ "undefined at: " ++ show (loc st) ++ " in findAction\nstate: " ++ prettyState 0 state ++ "\n: " ++ show (token st)

isAccepting :: State -> Token -> Bool
isAccepting state t  
    | t /= EOF = False 
    | otherwise = 
        let temp =  Set.foldl (\acc p -> 
                                if length (rule p) == pos p && EOF `elem` followSet p && rule p == rule startPosition then 
                                    Just p
                                else 
                                    acc
                            ) Nothing state
        in
            case temp of 
                Just _ -> True
                Nothing -> False

lastParse :: ParseStack -> Ast 
lastParse [Pa a] = a 
lastParse e = error $ "Parser did not output a tree\n" ++ show e

data ParseItem = Pt StreamToken | Ps Stm | O Op | E Expr Location | Pa Ast | Fun Function | TDecl TypeDecl | FunParams [(String, String)] | FunArgs [Expr]
    deriving (Show)
type ParseStack = [ParseItem]


logStack :: ParseStack -> Int -> Writer [String] (ParseStack, Int)
logStack ps i = writer ((ps, i), [show ps ++ ", toPop: " ++ show i])

types :: ParseStack -> (ParseItem, ParseStack)
types ps = 
    let typ a = case a of 
            (E (Ast.IExp s _) _) -> s 
            (Pt StreamToken {content=Str t}) -> t
            _ -> throw $ TypeNotParsed $ show a

    in case ps of 

        (_:tToken:_:Pt StreamToken {content = Str var}:_:_:FunParams pms:rest) ->
            (FunParams ((var, typ tToken):pms), rest)

        (_:tToken:_:Pt StreamToken {content = Str var}:_:rest) ->
            (FunParams [(var, typ tToken)], rest)

        (Pt StreamToken {content = Str retType}:_:FunParams pms:rest) -> 
            let fpms = FunParams . reverse $ ("", retType):pms in
            (fpms, rest)

        _ -> throw $ TypeNotParsed $ show ps

ruleFuncs :: Rule -> ParseStack -> Writer [String] (ParseStack, Int)
ruleFuncs input stack = 
    case (input, stack) of

         -- TopLevel: 
    
        
         ([V "Function"], f:rest) -> 
            logStack (f:rest) 1
         ([V "TypeDecl"], t:rest) -> 
            logStack (t:rest) 1

         -- type declarations: 
         ([T Ident, T Equal, T LeftBracket, V "TypeList", T RightBreacket], _:FunParams tl:_:_:Pt StreamToken {content = Str tname}:rest) -> 
            let tdecl = TypeDecl {tname=tname, typs=tl}
            in logStack (TDecl tdecl :rest) 5

         ([T Ident, T Colon, T Ident, T Comma, V "TypeList"], FunParams tl:_:Pt StreamToken {content = Str tname}:_:Pt StreamToken {content = Str name}:rest) -> 
            logStack (FunParams ((name, tname) : tl):rest) 5 
         ([T Ident, T Colon, T Ident], Pt StreamToken {content = Str name}: _ : Pt StreamToken {content = Str t}:rest) -> 
            logStack (FunParams [(t, name)]:rest) 3

         -- in types 
         ([T LeftSqBracket, T Ident, T RightSqBracket], _:Pt st@StreamToken {content = Str typ}:_:rest ) -> 
                 logStack (Pt StreamToken {token = token st, content = Str $ "ptr " ++ typ, loc = loc st}:rest) 3
         ([V "Exp", T LeftSqBracket, V "Exp", T RightSqBracket], _:E lup _:_:E lhs loc:rest) -> 
            let arrLup = E $ ArrLookUp lhs lup loc
            in logStack (arrLup loc:rest) 4
         ([T Literal, T Comma, V "ArrLit"], E e _:_:Pt st@StreamToken {content = I i}:rest) -> 
            let newFargs = FunArgs [LitExp (LI i) $ loc st, e] 
            in logStack (newFargs:rest) 3
         ([T Literal, T Comma, V "ArrLit"], FunArgs lits:_:Pt st@StreamToken {content = I i}:rest) -> 
            let newFargs = FunArgs $ LitExp (LI i) (loc st) : lits 
            in logStack (newFargs:rest) 3
         ([T LeftSqBracket, V "ArrLit", T RightSqBracket], _:FunArgs lits:Pt st:rest) -> 
            let arrLit = E $ ArrLit (reverse lits) $ loc st
            in logStack (arrLit (loc st):rest) 3
         ([T LeftSqBracket, V "ArrLit", T RightSqBracket], _:E l _:Pt st:rest) -> 
            let arrLit = E $ ArrLit [l] $ loc st
            in logStack (arrLit (loc st):rest) 3
         -- Var Instantiation
         ([T Var, T Ident, T Equal, V "Exp", T SemiColon], _:E rhs _:_:Pt st@StreamToken {content = Str ident}:_:rest) -> 
            let varInst = Ps $ VarInst ident rhs $ loc st
            in logStack (varInst:rest) 5
         -- Double EqualSign 
         ([T Equal, T Equal], _:_:rest) -> 
            logStack (O Eq:rest) 2
         -- Closure 
         ([T LeftParen, V "Scope", T RightParen], _:Ps scope:Pt lp:rest) -> 
            let clos = E (Closure scope $ loc lp) $ loc lp in
            logStack (clos:rest) 3
         ([V "Exp", T Comma, V "ArrLit"], E e _:_:E r _:rest) -> 
            let newFargs = FunArgs $ [e, r] 
            in logStack (newFargs:rest) 3
         ([V "Exp", T Comma, V "ArrLit"], E e _:_:FunArgs lits:rest) -> 
            let newFargs = FunArgs $ e:lits 
            in logStack (newFargs:rest) 3
         ([T LeftSqBracket, V "ArrLit", T RightSqBracket], _:FunArgs lits:Pt lsq:rest) -> 
            let l = loc lsq in
            let arrLit = E $ ArrLit lits l
            in logStack (arrLit l :rest) 3
         -- Var Instantiation
         ([T Var, T Ident, T Equal, V "Exp", T SemiColon], _:E rhs _:_:Pt StreamToken {content = Str ident, loc=l}:_:rest) -> 
            let varInst = Ps $ VarInst ident rhs l
            in logStack (varInst:rest) 5
         -- Double EqualSign 
         ([T Equal, T Equal], _:_:rest) -> 
            logStack (O Eq:rest) 2
         -- Closure 
         ([T LeftParen, V "Scope", T RightParen], _:Ps scope:Pt lp:rest) -> 
            let l = loc lp in
            let clos = E (Closure scope l) l in
            logStack (clos:rest) 3
         -- range
         ([V "Exp", T Dot, T Dot, V "Term"], E r _:_:_:E l lo:rest) -> 
            let range = E (Range l r lo) in
            logStack (range lo:rest) 4
         -- for-loops
         ([T For, T Ident, T In, V "Exp", T RightArrow, V "Exp", T LeftArrow], _:E body _:_:E iter _:_:Pt StreamToken {content = Str ident, loc= l}:_:rest) -> 
            let floop = Ps (ForLoop ident iter body l) in
            logStack (floop:rest) 7
         -- if-then-else statement

         ([T If, V "Exp", T Then, V "Exp", T Else, V "Exp"], E el _ : _ : E thn _ : _ : E condition _ : Pt i : rest) -> 
            let funcall = Ps (IfThenElse condition thn el $ loc i) in
            logStack (funcall:rest) 6
         -- fun call with no args
         ([V "Exp", T LeftParen, T RightParen], _: _ : E exp l : rest) -> 
            let funcall = E (FunCall exp [] l) in
            logStack (funcall l:rest) 3
         ([V "Exp", T LeftParen, V "Fparams", T RightParen], _:E arg _ : _ : E exp l : rest) -> 
            let funcall = E (FunCall exp [arg] l) in
            logStack (funcall l:rest) 4
         ([V "Exp", T LeftParen, V "Fparams", T RightParen], _:FunArgs args : _ : E exp l : rest) -> 
            let funcall = E (FunCall exp args l) in
            logStack (funcall l:rest) 4
         ([V "Exp", T Comma, V "Fparams"], E l _:_:E e _:rest) -> 
            let args = FunArgs [e, l] in
            logStack (args:rest) 3
         ([V "Exp", T Comma, V "Fparams"], FunArgs lits:_:E e _:rest) -> 
            let args = FunArgs $ e:lits in
            logStack (args:rest) 3
         ([V "Types", T RightArrow, T Ident], stack) ->  
            let (pms, rest) = types stack in 
            logStack (pms:rest) 3
         ([V "Types", T RightArrow, T LeftParen, T Ident, T Colon, V "Type", T RightParen], stack) ->  
            let (pms, rest) = types stack in 
            logStack (pms:rest) 7
         ([T LeftParen, T Ident, T Colon, V "Type", T RightParen], stack) ->  
            let (pms, rest) = types stack in 
            logStack (pms:rest) 5
-- for not params and only a return type
         ([T Ident, T Colon, V "ReturnTypes", V "Scope"] ,Ps scope:E e _: _: Pt StreamToken {content = Str name}:rest) -> 
            logStack (Fun Function {funName = name, params = [], body = scope, returnType = show e}:rest) 4
-- for multiple params
         ([T Ident, T Colon, V "ReturnTypes", V "Scope"] ,Ps scope:FunParams fps: _: Pt StreamToken {content = Str name}:rest) -> 
            let (ret, pms) = case reverse fps of 
                    ((_,rt): ps) -> (rt, ps)
                    [] -> ("Thunk", [])
                fun = Function {funName = name, params = reverse pms, body = scope, returnType = ret}

            in logStack (Fun fun :rest) 4
         ([T LeftParen, T RightParen] ,_:_:rest) ->  
            logStack (FunParams []:rest) 2

         ([V "Ast", V "TopLevel"] , TDecl t:Pa ast:rest) ->  
            logStack (Pa (ast {tdecls = t : tdecls ast}):rest) 2
         ([V "Ast", V "TopLevel"] , Fun f:Pa ast:rest) ->  
            logStack (Pa (ast {fdecls = f : fdecls ast}):rest) 2

         -- end
         ([V "Ast"] ,Pa a:_) ->  
            logStack [Pa a] 1
         ([V "TopLevel"], TDecl t:rest) -> 
            logStack (Pa Ast {fdecls = [], tdecls = [t]}:rest) 1
         ([V "TopLevel"], Fun f:rest) -> 
            logStack (Pa Ast {fdecls = [f], tdecls = []}:rest) 1

         ([T Let, T Ident, T Equal, V "Exp", T In] ,_:E e _:_:Pt StreamToken {content = Str str, loc = l}:_:rest) ->  
            logStack (Ps (LetIn str e l):rest)  5
         ([T LeftParen, V "Exp", T RightParen] ,_:(E e l):_:rest) ->  
            logStack (E e l:rest)  3
         ([V "Term"] ,(E term l):rest) ->  
            logStack (E term l:rest) 1
         ([V "Exp", V "Op", V "Term"] ,(E rhs _):(O o):(E lhs l):rest) ->  
            logStack (E (BinOp lhs o rhs l) l:rest) 3 
         ([V "Exp"] ,E e l:rest ) -> 
            logStack (E e l:rest) 1 
         ([V "Function"] ,Pa a:rest) ->  
            logStack (Pa a:rest) 1
         ([T Ident, T Colon, V "Types", V "Scope"] ,Ps scope:FunParams fps :_: Pt StreamToken{content = Str fname}:rest) ->  -- Function declarations
            logStack (Fun Function {funName = fname, params = fps, body = scope}:rest) 4
         ([V "Scope", V "Stm"] ,Ps e:Ps (Scope a):rest) ->  
            logStack (Ps (Scope (a ++ [e])):rest) 2
         ([V "Scope", V "Stm"] ,Ps e:Ps a:rest) ->  
            logStack (Ps (Scope [a, e]):rest) 2
         ([V "Stm"] ,Ps e:rest) ->  
            logStack (Ps e:rest) 1
         ([V "Stm"], [Ps e]) -> 
            logStack [Ps e] 1
         ([T Lexer.Slash] ,Pt _:rest) ->   
            logStack (O Ast.Div:rest) 1
         ([T Lexer.Plus] ,Pt _:rest) ->   
            logStack (O Ast.Plus:rest) 1
         ([T Lexer.Star] ,Pt _:rest) ->   
            logStack (O Ast.Mult:rest) 1
         ([T Lexer.Return, V "Exp"] ,(E e l):(Pt _):rest) ->  
            logStack (Ps (Ast.Return e l):rest) 2
         ([T Lexer.Minus] ,Pt _:rest) ->   
            logStack (O Ast.Minus:rest) 1
         ([T Lexer.Lt] ,Pt _:rest) ->   
            logStack (O Ast.Lt:rest) 1
         ([T Lexer.Lte] ,Pt _:rest) ->   
            logStack (O Ast.Lte:rest) 1
         ([T Lexer.Gt] ,Pt _:rest) ->   
            logStack (O Ast.Gt:rest) 1
         ([T Lexer.Gte] ,Pt _:rest) ->   
            logStack (O Ast.Gte:rest) 1
         ([T Lexer.Gte] ,Pt _:rest) ->   
            logStack (O Ast.Eq:rest) 1
         ([T Lexer.Ident] ,Pt StreamToken{content = Str str, loc=l}:rest) ->  
                    logStack (E (IExp str l) l:rest) 1
         ([T Lexer.Literal] ,Pt StreamToken{content = I i, loc=l}:rest) ->  
                        logStack (E (LitExp  (LI i) l) l:rest) 1
         (r, ps) ->  throw $ MissingRule $ "Undefined parse error\nrule: " ++ show r ++ "\non stack: " ++ show ps

-------------------- Generating Grammar --------------------

parseGrammar :: String -> Grammar 
parseGrammar s = 
    let ls = Prelude.filter (/= "") $ lines s in
    let asTxt = pack <$> ls in
    Prelude.foldl (\acc t -> 
        Dm.unionWith (++) (parseLine t) acc
    ) Dm.empty asTxt

parseLine :: Text -> Grammar 
parseLine text = 
    let fs = splitOn (pack "=>") text in 
    case fs of 
        [l, r] -> 
            let l' = unpack $ Data.Text.takeWhile (/= ' ') l in 
            let r' = fmap parseAtom $ Prelude.filter (/= pack "") $ splitOn (pack " ") r in  
            Dm.singleton l' [r']
        e -> error $ "kodivslænk" ++ show e ++ "\n" ++ show text

parseAtom :: Text -> Atom 
parseAtom text 
    | Data.Text.head text `elem` ['A'..'Z'] = 
        V (unpack text)
    | otherwise = case unpack text of 
        "literal" -> T Literal 
        "if"      -> T If
        "then"    -> T Then 
        "else"    -> T Else
        "ident"   -> T Ident 
        "plus"    -> T Lexer.Plus 
        "minus"   -> T Lexer.Minus
        "mult"    -> T Lexer.Star
        "slash"   -> T Lexer.Slash
        "return"  -> T Lexer.Return
        "("       -> T LeftParen 
        ")"       -> T RightParen
        "["       -> T LeftSqBracket 
        "]"       -> T RightSqBracket
        "{"       -> T LeftBracket 
        "}"       -> T RightBreacket
        "let"     -> T Let
        "in"      -> T In
        "var"     -> T Var
        "="       -> T Equal 
        ";"       -> T SemiColon
        ":"       -> T Colon
        ","       -> T Comma
        "->"      -> T RightArrow 
        "<-"      -> T LeftArrow
        "lt"      -> T Lexer.Lt
        "lte"     -> T Lexer.Lte
        "gt"      -> T Lexer.Gt
        "gte"     -> T Lexer.Gte
        "eq"      -> T Lexer.Equal
        "for"     -> T For
        "."       -> T Dot
        "type"       -> T Type
        e         -> error $ "mising data types to parse to for: " ++ e
    
