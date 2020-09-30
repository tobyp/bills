import Text.Parsec
import Text.Parsec.Expr
import Text.Printf
import Data.Ratio
import Data.Maybe
import Data.Functor.Identity
import qualified Data.Map.Strict as M
import GHC.Exts
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO

kVERSION = "2.0.3"

type Person = String
type Weight = Ratio Integer
type Money = Ratio Integer

data Share = Share Person Weight deriving Show
data Account = Account Person Money deriving Show
data Transaction = Transaction Person Money deriving Show

data ParserState = ParserState {
    psDefineMap :: M.Map Person [Share],
    psVariables :: M.Map String Money,
    psAllowPlaceholder :: Bool,
    psAllowUnterminated :: Bool,
    psShowFlows :: Bool
}
initParserState :: ParserState
initParserState = ParserState {
    psDefineMap=M.empty,
    psVariables=M.empty,
    psAllowPlaceholder=False,
    psAllowUnterminated=False,
    psShowFlows=False
}

psDefine name shares ps@(ParserState{psDefineMap=defs}) = ps {psDefineMap=M.insert name shares defs}
psUndefine name ps@(ParserState{psDefineMap=defs}) = ps {psDefineMap=M.delete name defs}
psSet name value ps@(ParserState{psVariables=vars}) = ps {psVariables=M.insert name value vars}
psUnset name ps@(ParserState{psVariables=vars}) = ps {psVariables=M.delete name vars}
psGet name ps@(ParserState{psVariables=vars}) = M.lookup name vars

mkShares :: ParserState -> Person -> [Share]
mkShares m p = M.findWithDefault [Share p 1] p (psDefineMap m)

sharesExclude :: [Share] -> [Share] -> [Share]
sharesExclude included excluded = normalizeShares [s | s@(Share p _) <- included, not (p `elem` excludedPersons)]
    where
        excludedPersons = [p | (Share p _) <- excluded]

nameStart = oneOf "*ABCDEFGHIJKLMNOPQRSTUVWXYZ"
nameFollow = oneOf "_abcdefghijklmnopqrstuvwxyz"
varStart = oneOf "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz_"
varFollow = oneOf "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz_0123456789'"
whitechar = oneOf " \t\v"
whitespace = many1 whitechar >> return ()
optWhitespace = many whitechar >> return ()
decimalSeparator = oneOf ".,"

-- String
person = do
    head <- nameStart
    tail <- many nameFollow
    return $ head:tail

-- Ratio Integer
number = do
    intpart <- many1 digit
    fracpart <- option (0 % 1) $ do {
        decimalSeparator;
        fracpart <- many1 digit;
        return $ (read fracpart) % (10 ^ (length fracpart)) }
    return $ (read intpart) % 1 + fracpart


data Expr a
    = ExprLit a
    | ExprVar String
    | ExprUnOp (a -> a) (Expr a)
    | ExprBinOp (a -> a -> a) (Expr a) (Expr a)


eval :: M.Map String a -> Expr a -> Either String a
eval scope (ExprLit a) = Right a
eval scope (ExprVar n) = case M.lookup n scope of
    Just a -> Right a
    Nothing -> Left $ "No such variable \"" ++ n ++ "\""
eval scope (ExprUnOp op arg) = op <$> eval scope arg
eval scope (ExprBinOp op argl argr) = op <$> eval scope argl <*> eval scope argr

instance Num a => Num (Expr a) where
    (+) = ExprBinOp (+)
    (-) = ExprBinOp (-)
    (*) = ExprBinOp (*)
    negate = ExprUnOp negate
    abs = ExprUnOp abs
    signum = ExprUnOp signum
    fromInteger = ExprLit . fromInteger

instance Fractional a => Fractional (Expr a) where
    fromRational = ExprLit . fromRational
    (/) = ExprBinOp (/)

-- Money == Ratio Integer

variableName = do
    head <- varStart
    tail <- many varFollow
    return $ head:tail

expr_lit = ExprLit <$> number

expr_var = ExprVar <$> variableName

expr_paren = do
    char '('
    optWhitespace
    e <- expr
    optWhitespace
    char ')'
    return e

expr_term = do
    e <- try expr_paren <|> try expr_lit <|> expr_var
    optWhitespace
    return e

expr_binop_mul = do { char '*'; optWhitespace; return (*) }
expr_binop_div = do { char '/'; optWhitespace; return (/) }
expr_binop_add = do { char '+'; optWhitespace; return (+) }
expr_binop_sub = do { char '-'; optWhitespace; return (-) }
expr_unop_neg = do { char '-'; optWhitespace; return negate }

expr_ops
    = [
        [Prefix expr_unop_neg],
        [Infix expr_binop_mul AssocLeft, Infix expr_binop_div AssocLeft],
        [Infix expr_binop_add AssocLeft, Infix expr_binop_sub AssocLeft]
    ]

expr = buildExpressionParser expr_ops expr_term

money = do
    e <- expr
    ps <- getState
    case eval (psVariables ps) e of
        Left error -> fail error
        Right result -> return result

-- [Share]
share_person = do
    p <- person
    s <- getState
    return $ mkShares s p

share_placeholder = do
    char '?'
    state <- getState
    if psAllowPlaceholder state
        then return []
        else fail "Found placeholder (Entries containing a ? are not allowed)"

-- [Share]
share_paren = between (char '(') (char ')') shareDifference

-- [Share]
share = do
    weight <- option (1) number
    shares <- try share_person <|> try share_placeholder <|> share_paren
    return $ scaleShare (1 / weight) <$> shares

-- [Share]
shareUnion = do
    shares <- sepBy1 share (optional $ char ',')
    return $ normalizeShares $ concat shares

-- [Share]
shareDifference = do
    sharesIncluded <- shareUnion
    sharesExcluded <- option [] $ do
        char '\\'
        shareUnion
    return $ sharesExclude sharesIncluded sharesExcluded

-- ()
comment = do
    char '#'
    many $ noneOf "\n"

--  [Transaction]
line_define = do
    string "%define"
    whitespace
    token <- person
    whitespace
    shares <- shareDifference
    optWhitespace
    optional comment
    modifyState $ psDefine token $ normalizeShares shares
    return []

--  [Transaction]
line_undefine = do
    string "%undefine"
    whitespace
    token <- person
    optWhitespace
    optional comment
    modifyState $ psUndefine token
    return []

-- [Transaction]
line_exchange = do
    string "%set"
    whitespace
    var <- variableName
    whitespace
    value <- number
    optWhitespace
    optional comment
    modifyState $ psSet var value
    return []

-- [Transaction]
line_currency = do
    string "%unset"
    whitespace
    var <- variableName
    optWhitespace
    optional comment
    modifyState $ psUnset var
    return []

-- [Transaction]
line_entry = do
    optWhitespace
    creditors <- shareDifference
    many1 space
    debtors <- shareDifference
    terminated <- optionMaybe (char '.')
    state <- getState
    transactions <- case (terminated, psAllowUnterminated state) of
        (Nothing, False) ->
            fail "Unterminated entry (entries without a . at the end are not allowed)"
        _ -> do
            skipMany space
            amount <- money
            return $ shareTransaction creditors amount debtors
    optWhitespace
    optional comment
    return transactions

-- [Transaction]
line_empty = do
    optWhitespace
    optional comment
    return []

-- [Transaction]
line = try line_define <|> try line_undefine <|> try line_exchange <|> try line_currency <|> try line_entry <|> line_empty

eol = (try $ string "\n\r") <|> (try $ string "\r\n") <|> (try $ string "\n")

-- [Transaction]
bills = do
    lines <- sepEndBy line eol
    eof
    return $ concat lines

scaleShare :: Weight -> Share -> Share
scaleShare scalar (Share p w) = Share p (w / scalar)

-- scale shares linearly until the sum of all share weights is 1
normalizeShares :: [Share] -> [Share]
normalizeShares shares = scaleShare total <$> shares
    where
        total = foldl (+) (0 % 1) $ shareWeight <$> shares
        shareWeight (Share _ w) = w

-- split a multiple-shares transaction into separate one-person transactions
-- shareTransaction creditor_shares amount debtor_shares = one_person_transactions
shareTransaction :: [Share] -> Money -> [Share] -> [Transaction]
shareTransaction = go []
    where
        go txs [] _ [] = txs
        go txs [] m ((Share p w):ds) = go ((Transaction p (w * m)):txs) [] m ds
        go txs ((Share p w):cs) m ds = go ((Transaction p (-w * m)):txs) cs m ds

-- apply a transaction to a list of accounts: before -> transaction -> after
applyTransaction :: [Account] -> Transaction -> [Account]
applyTransaction [] (Transaction p m) = [Account p m]
applyTransaction (a@(Account ap am):as) t@(Transaction p m)
    | ap == p = (Account p (m + am)):as
    | otherwise = a:(applyTransaction as t)

-- recursively calculate transfers from the biggest debitor to the biggest creditor until no credits/debits are left.
-- this algorithm only works because we always know there will be at least two accounts (otherwise somebody would owe, but nobody would be owed) and the sum of all credits+debits is 0
collapseAccounts :: [Account] -> IO ()
collapseAccounts [] = return ()
collapseAccounts accs = do
    printf "todo: %s pays %f to %s\n" debitor (((fromRational baseline) :: Float)) creditor
    collapseAccounts $ filter accountNotEmpty $ modifiedAccounts
        where
            accountNotEmpty (Account _ m) = m /= 0
            modifiedAccounts = (Account creditor (credit + baseline)):(Account debitor (debit - baseline)):otherAccs
            baseline = min (-credit) debit
            ((Account creditor credit), (Account debitor debit), otherAccs) = minMaxAccounts accs

-- total inflow and outflow of every person's wallet
totalFlow :: [Transaction] -> [(Person, Money, Money)]
totalFlow ts = flatten <$> (M.assocs $ go M.empty ts)
    where
        go m_ [] = m_
        go m_ ((Transaction p m):ts) = go (M.alter (recordFlow m) p m_) ts
        recordFlow m Nothing = Just $ if m < 0 then (-m, 0) else (0, m)
        recordFlow m (Just (mi, mo)) = Just $ if m < 0 then (mi - m, mo) else (mi, mo + m)
        flatten (p, (mi, mo)) = (p, mi, mo)

-- separate out the biggest creditor and debitor from an account list (of length >= 2!)
minMaxAccounts :: [Account] -> (Account, Account, [Account])
minMaxAccounts [] = error "no accounts"
minMaxAccounts (_:[]) = error "not enough accounts"
minMaxAccounts accs = (minAcc, maxAcc, restAccs)
    where
        sortedAccs = sortWith (\(Account _ m) -> m) accs
        (minAcc:[], restMaxAccs) = splitAt 1 sortedAccs
        (restAccs, maxAcc:[]) = splitAt (length restMaxAccs - 1) restMaxAccs


data BillsCliFlag = FlagHelp | FlagVersion | FlagAllowPlaceholder | FlagAllowUnterminated | FlagShowFlows deriving Eq
billsCliFlags = [
    Option ['h'] ["help"] (NoArg FlagHelp) "Show help message",
    Option ['v'] ["version"] (NoArg FlagVersion) "Show version information",
    Option ['P'] ["allow-placeholder"] (NoArg FlagAllowPlaceholder) "Allow placeholders (as if they weren't there)",
    Option ['T'] ["allow-unterminated"] (NoArg FlagAllowUnterminated) "Allow unterminated input",
    Option ['F'] ["show-flows"] (NoArg FlagShowFlows) "Show total flows per person"
    ]

billsCliParse argv = case getOpt Permute billsCliFlags argv of
    (flags, filenames, []) -> do
        let filenames' = if null filenames then ["-"] else filenames
        if FlagHelp `elem` flags then do
            hPutStrLn stderr $ usageInfo billsCliUsage billsCliFlags
            exitWith ExitSuccess
        else if FlagVersion `elem` flags then do
            hPutStrLn stderr billsCliVersion
            exitWith ExitSuccess
        else do
            let ps = initParserState {
                psAllowUnterminated=FlagAllowUnterminated `elem` flags,
                psAllowPlaceholder=FlagAllowPlaceholder `elem` flags,
                psShowFlows=FlagShowFlows `elem` flags
            }
            return (ps, filenames')
    (_, _, errs) -> do
        hPutStrLn stderr $ concat errs ++ usageInfo billsCliUsage billsCliFlags
        exitWith $ ExitFailure 1
    where
        billsCliUsage = "Usage: bills [--help] [--version] [--allow-placeholder] [--allow-unterminated] [files...]"
        billsCliVersion = "Bills " ++ kVERSION ++ ", (c) 2015-2020 tobyp <tobyp@tobyp.net>"

getFileContents "-" = getContents
getFileContents filename = readFile filename

parseBill parserState filename = do
    content <- getFileContents filename
    case runParser bills parserState filename content of
        Left err -> do
            hPutStrLn stderr "Parsing failed:"
            hPutStrLn stderr $ show err
            exitWith $ ExitFailure 1
        Right transactions -> do
            return transactions

printFlow :: (Person, Money, Money) -> IO ()
printFlow (p, m1, m2) = printf "flow: %s consumed %f and %s %f (so, is out %f)\n" p consumed relation (abs diff) isout
    where
        consumed = (fromRational m2) :: Float
        isout = (fromRational m1) :: Float
        diff = (fromRational (m1 - m2)) :: Float
        relation = if diff > 0 then "is owed" else "owes"

main :: IO ()
main = do
    (parserState, filenames) <- getArgs >>= billsCliParse
    let bills = parseBill parserState <$> filenames
    transactions <- concat <$> sequence bills
    if psShowFlows parserState then do {
        sequence $ printFlow <$> totalFlow transactions
    } else return [()]
    collapseAccounts $ foldl applyTransaction [] transactions
    exitWith $ ExitSuccess
