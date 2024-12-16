module Parser (testParser) where 
import Data.Set as Set
import Lexer (Token(..))
import Data.Bifunctor (Bifunctor(bimap))
import qualified Data.Map.Strict as Dm

type Rule = [Atom]

data Atom = T Token | V String 
    deriving (Ord, Eq, Show)

type Grammar = Dm.Map String [Rule]



-- >>>  test
-- fromList [("Exp",[[V "Exp",V "Op",V "Term"],[V "Term"]]),("Op",[[T Plus],[T Minus]]),("Stm",[[V "Stm",V "Exp"],[V "Exp"]]),("Term",[[T (Literal {num = 1})],[T (Ident {ident = "String"})],[T LeftParen,V "Exp",T RightParen]])]
test :: Grammar
test = 
    let terms = Dm.singleton "Term" [[T Literal {num=1}], [T Ident {ident="String"}], [T LeftParen, V "Exp", T RightParen]]
    in 
    let ops = Dm.insert "Op" [[T Plus], [T Minus]] terms in
    let exp = 
            Dm.insert "Exp" [[V "Exp", V "Op", V "Term"]
            , [V "Term"]
            ] ops
    in
    let stm = Dm.insert "Stm" [[V "Stm", V "Exp"], [V "Exp"]] exp in
    stm


testParser :: IO () 
testParser = do 
    let t = test 
    print $ findTable t 
    print $ followTable t

isToken :: Atom -> Bool 
isToken (T _) = True 
isToken (V _) = False 

stringAtomEq :: String -> Atom -> Bool 
stringAtomEq s (V a) = s == a 
stringAtomEq _ _ = False

findTable :: Grammar -> Dm.Map String (Set Token) 
findTable g = Dm.foldlWithKey (\acc k v ->   Dm.insert k (find k v g) acc) Dm.empty g  


followTable :: Grammar -> Dm.Map String (Set Token)
followTable g = 
    let (nonTerms, rules) = unzip $ Dm.toList g in 
    let initial = Prelude.foldl (\acc nt ->
            let relevantRules = concat . Prelude.filter (elem (V nt)) <$> rules in
            let alsoRelevant = concatMap snd . Prelude.filter (\(s, _) -> s == nt) $ Dm.toList g in
            Dm.unionWith Set.union acc (foo Set.empty nt (relevantRules ++ alsoRelevant) g) ) Dm.empty nonTerms 
    in
    initial
--Todo Write a fix-point function
followFixPoint :: Grammar -> Dm.Map String (Set Token) -> Dm.Map String (Set Token)
followFixPoint g = id


findRelevant :: String -> [[Rule]] -> Grammar -> [Rule]
findRelevant nt rules g = 
        let relevantRules = concat . Prelude.filter (elem (V nt)) <$> rules in
        let b = concatMap snd . Prelude.filter (\(s, _) -> s == nt) $ Dm.toList g in 
        relevantRules ++ b

--insertIntoMap :: 
-- >>> foo Set.empty "Stm" [[V "Stm", V "Exp"], [V "Exp"]] test
-- fromList [("Exp",fromList [Ident {ident = "String"},LeftParen,Literal {num = 1}]),("Stm",fromList [Ident {ident = "String"},LeftParen,Literal {num = 1}])]
foo :: Set String ->  String -> [Rule] -> Grammar -> Dm.Map String (Set Token)
foo acc s [] _ = Dm.singleton s Set.empty
foo acc s rs g = 
    let (trivialFollows, nested, followed) = 
         Prelude.foldl (\(mAcc, sAcc, facc) r -> 
            let (tokens, vars, followVars) = bar s r in
            (mAcc `union` tokens, sAcc `union` vars, facc `union` followVars)
         ) (Set.empty, Set.empty, Set.empty) rs
    in
    let firstMap = Dm.singleton s trivialFollows in 
    let nested' = Set.difference nested acc in
    let res = Set.foldl (\acc2 var ->
                let lup = Dm.lookup var g in
                let next = Set.empty `maybe` (\v -> find var v g)  in
                acc2 `union` next lup 
            ) trivialFollows nested'
    in
    let map' = Dm.insert s res firstMap in 
    let copy = 
            let lup = Dm.findWithDefault Set.empty s map' in 
            Set.foldl (\acc' f -> Dm.insert f lup acc') Dm.empty followed
    in
    Dm.unionWith Set.union map' copy 

foobar :: Set String -> Set String -> [[Rule]] -> Grammar -> Dm.Map String (Set Token)
foobar acc' followed rules g = Set.foldl (\acc2 var -> 
                let rel = findRelevant var rules g in
                foo acc' var rel g `Dm.union` acc2
            ) Dm.empty followed


bar :: String -> Rule -> (Set Token, Set String, Set String)
bar s [V va]  
    | va == s = (Set.singleton EOF, Set.empty, Set.empty)
    | va /= s = (Set.empty, Set.empty, Set.singleton va)
    | otherwise = (Set.empty, Set.empty, Set.empty)
bar s ((V va):(T tb):rest)  
    | va == s = 
        let (newA, newB, c) = bar s rest in 
        (Set.singleton tb `union` newA, newB, c)
    | otherwise = 
        bar s rest   
bar s ((V va):(V vb):rest) 
    | va == s && vb /= s =
        let (a, b, c) = bar s rest  in
        (a, Set.singleton vb `union` b, c)
    | otherwise = bar s $ V vb:rest 
bar s ((T _):rest) = bar s rest
bar _ _ = (Set.empty, Set.empty, Set.empty)


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




