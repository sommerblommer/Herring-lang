module Parser (testParser) where 
import Data.Set as Set
import Lexer (Token(..))
import qualified Data.Map.Strict as Dm
import Data.Maybe (catMaybes)
import qualified Data.Foldable as D

type Rule = [Atom]

data Atom = T Token | V String 
    deriving (Ord, Show)

instance Eq Atom where 
    (T a) == (T b) = a == b 
    (V a) == (V b) = a == b
    _ == _ = False


type Grammar = Dm.Map String [Rule]


startRule :: String 
startRule = "Stm"

testParser :: IO () 
testParser = do 
    putStrLn ":::::::"
    putStrLn "FindTable:"
    print $ findTable test
    putStrLn ":::::::"
    putStrLn "FollowTable"
    let res =  followTable test
    print res 
    putStrLn ":::::::"
    let p = Position {prod = "S", rule = [V "S", V "A"], pos = 0, followSet = fromList [EOF]} 
    let e =  Position {prod = "", rule = [V "Stm"], pos = 0, followSet = fromList [EOF]} 
    --putStr $ prettyAuto . transition simpleGrammar . mergeClosures $ initialClosure simpleGrammar p 
    putStr $ prettyAuto . transition test . mergeClosures $ initialClosure test e 

simpleGrammar :: Grammar 
simpleGrammar = 
    let s = Dm.singleton "S" [[V "S", V "A"], [V "S", V "B"]] in
    let a = Dm.insert "A" [[T Ident {ident="a"}]] s in 
    Dm.insert "B" [[T Ident {ident="b"}]] a  

test :: Grammar
test = 
    let terms = Dm.singleton "Term" [[T Literal {num=1}], [T Ident {ident="String"}], [T LeftParen, V "Exp", T RightParen]]
    in 
    let ops = Dm.insert "Op" [[T Plus], [T Minus]] terms in
    let exps = 
            Dm.insert "Exp" [[V "Exp", V "Op", V "Term"]
            , [V "Term"]
            ] ops
    in
    let stm = Dm.insert "Stm" [[V "Stm", V "Exp"], [V "Exp"]] exps in
    stm

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
prettyAuto = Dm.foldlWithKey (\acc k v -> acc ++ prettyState k ++ "with transitions\n" ++ prettyAA v) "Automaton:\n" 

prettyAA :: Dm.Map Atom State -> String 
prettyAA  = Dm.foldlWithKey (\acc k v -> acc ++ "transition: " ++ pretty k ++ "\n" ++ prettyState v) "" 

prettyState :: State -> String
prettyState = Set.foldl (\acc p -> acc ++ pretty p ++ "\n") ""

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
    pretty (T Ident {ident=_}) = "Ident"
    pretty (T Literal {num=_}) = "Literal"
    pretty (T t) = show t
    pretty (V v) = v
-------------------- GOTO --------------------
type Automaton = Dm.Map State (Dm.Map Atom State)

--  findTransitionTargets . mergeClosures . initialClosure test $ Position {prod = "Stm", rule = [V "Stm"], pos = 0, followSet = fromList [EOF]}


--  >>> prettyAuto . transition simpleGrammar . mergeClosures . initialClosure simpleGrammar $ Position {prod = "S", rule = [V "S", V "A"], pos = 0, followSet = fromList [EOF]}
-- "S ->   S A\nS ->   S B\n\nS\nA ->   Ident {ident = \"a\"}\nB ->   Ident {ident = \"b\"}\nS ->   S A\nS ->   S B\n"

-- closure simpleGrammar $ fromList [Position {prod = "S", rule = [V "S",V "A"], pos = 1, followSet = fromList [Ident {ident = "a"},Ident {ident = "b"},EOF]}]
-- fromList [Position {prod = "A", rule = [T (Ident {ident = "a"})], pos = 0, followSet = fromList [Ident {ident = "a"},Ident {ident = "b"},EOF]},Position {prod = "S", rule = [V "S",V "A"], pos = 1, followSet = fromList [Ident {ident = "a"},Ident {ident = "b"},EOF]}]

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


transition :: Grammar -> State -> Automaton 
transition g state = 
    let targets = findTransitionTargets state in 
    let a = Prelude.foldl (\acc (a, set) -> 
                                let newState = closure g set in
                                let newM = Dm.singleton a newState in 
                                Dm.union acc newM
                            ) Dm.empty $ Dm.toList targets
    in
    Dm.singleton state a 

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
-- fromList [Position {prod = "S", rule = [V "S"], pos = 0, followSet = fromList [EOF]},Position {prod = "S", rule = [V "S",V "A"], pos = 0, followSet = fromList [Ident {ident = "a"},Ident {ident = "b"},EOF]},Position {prod = "S", rule = [V "S",V "B"], pos = 0, followSet = fromList [Ident {ident = "a"},Ident {ident = "b"},EOF]}]


--   mergeClosures $ closure test $ Position {prod = "Stm", rule = [V "Stm"], pos = 1, followSet = fromList [EOF]}

mergeClosures :: State -> State 
mergeClosures = helper . toList where 
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

closureR:: Grammar -> Position -> State
closureR g p 
    | length (rule p) == pos p = Set.singleton p 
    | otherwise = 
        let locus = rule p !! pos p in
        case locus of 
            T _ -> Set.empty
            V var -> 
                let lup = g Dm.! var in
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
    let l = g Dm.! s in 
    find s l g

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
