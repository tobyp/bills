import Text.Parsec
import Text.Printf
import Data.Ratio
import Data.Maybe
import qualified Data.Map.Strict as M
import GHC.Exts
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO

kVERSION = "2.0.0"

type Person = String
type Money = Ratio Integer
type Weight = Ratio Integer

data Share = Share Person Weight deriving Show
data Account = Account Person Money deriving Show
data Transaction = Transaction Person Money deriving Show

data ParserState = ParserState {
    psDefineMap :: M.Map Person [Share],
    psAllowPlaceholder :: Bool,
    psAllowUnterminated :: Bool
}
initParserState :: ParserState
initParserState = ParserState {
    psDefineMap=M.empty,
    psAllowPlaceholder=False,
    psAllowUnterminated=False
}

psDefine p s ps@(ParserState{psDefineMap=m}) = ps {psDefineMap=M.insert p s m}
psUndefine p ps@(ParserState{psDefineMap=m}) = ps {psDefineMap=M.delete p m}

mkShares :: ParserState -> Person -> [Share]
mkShares m p = M.findWithDefault [Share p 1] p (psDefineMap m)

sharesExclude :: [Share] -> [Share] -> [Share]
sharesExclude included excluded = normalizeShares [s | s@(Share p _) <- included, not (p `elem` excludedPersons)]
    where
        excludedPersons = [p | (Share p _) <- excluded]

large = oneOf "*ABCDEFGHIJKLMNOPQRSTUVWXYZ"
small = oneOf "_abcdefghijklmnopqrstuvwxyz"
whitespace = oneOf " \t\v"
decimalSeparator = oneOf ".,"

-- String
person = do
    a <- large
    b <- many small
    return (a:b)

-- Ratio Integer
number = do
    sign <- option (1) ((char '-') >> (return (-1 % 1)))
    intpart <- many1 digit
    fracpart <- option (0 % 1) $ do {
        decimalSeparator;
        fracpart <- many1 digit;
        return $ (read fracpart) % (10 ^ (length fracpart)) }
    return $ (read intpart) % 1 + fracpart

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

-- [Transaction]
entry = do
    creditors <- shareDifference
    many1 space
    debtors <- shareDifference
    terminated <- optionMaybe (char '.')
    state <- getState
    case (terminated, psAllowUnterminated state) of
        (Nothing, False) ->
            fail "Unterminated entry (entries without a . at the end are not allowed)"
        _ -> do
            many1 space
            amount <- number
            return $ shareTransaction creditors amount debtors

-- Maybe [Transaction]
line_define = do
    string "%define"
    skipMany whitespace
    token <- person
    skipMany whitespace
    shares <- shareDifference
    skipMany whitespace
    optional comment
    modifyState $ psDefine token $ normalizeShares shares
    return Nothing

-- Maybe [Transaction]
line_undefine = do
    string "%undefine"
    skipMany whitespace
    token <- person
    skipMany whitespace
    optional comment
    modifyState $ psUndefine token
    return Nothing

-- Maybe [Transaction]
line_entry = do
    skipMany whitespace
    e <- optionMaybe entry
    skipMany whitespace
    optional comment
    return e

-- Maybe [Transaction]
line = try line_define <|> try line_undefine <|> line_entry

eol = (try $ string "\n\r") <|> (try $ string "\r\n") <|> (try $ string "\n")

-- [Transaction]
bills = do
    lines <- sepEndBy line eol
    eof
    return $ concat $ catMaybes lines

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
    printf "%s pays %f to %s\n" debitor (((fromRational baseline) :: Float)) creditor
    collapseAccounts $ filter accountNotEmpty $ modifiedAccounts
        where
            accountNotEmpty (Account _ m) = m /= 0
            modifiedAccounts = (Account creditor (credit + baseline)):(Account debitor (debit - baseline)):otherAccs
            baseline = min (-credit) debit
            ((Account creditor credit), (Account debitor debit), otherAccs) = minMaxAccounts accs

-- separate out the biggest creditor and debitor from an account list (of length >= 2!)
minMaxAccounts :: [Account] -> (Account, Account, [Account])
minMaxAccounts [] = error "no accounts"
minMaxAccounts (_:[]) = error "not enough accounts"
minMaxAccounts accs = (minAcc, maxAcc, restAccs)
    where
        sortedAccs = sortWith (\(Account _ m) -> m) accs
        (minAcc:[], restMaxAccs) = splitAt 1 sortedAccs
        (restAccs, maxAcc:[]) = splitAt (length restMaxAccs - 1) restMaxAccs

data BillsCliFlag = FlagHelp | FlagVersion | FlagAllowPlaceholder | FlagAllowUnterminated deriving Eq
billsCliFlags = [
    Option ['h'] ["help"] (NoArg FlagHelp) "Show help message",
    Option ['v'] ["version"] (NoArg FlagVersion) "Show version information",
    Option ['P'] ["allow-placeholder"] (NoArg FlagAllowPlaceholder) "Allow placeholders (as if they weren't there)",
    Option ['T'] ["allow-unterminated"] (NoArg FlagAllowUnterminated) "Allow unterminated input"
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
                psAllowPlaceholder=FlagAllowPlaceholder `elem` flags
            }
            return (ps, filenames')
    (_, _, errs) -> do
        hPutStrLn stderr $ concat errs ++ usageInfo billsCliUsage billsCliFlags
        exitWith $ ExitFailure 1
    where
        billsCliUsage = "Usage: bills [--help] [--version] [--allow-placeholder] [--allow-unterminated] [files...]"
        billsCliVersion = "Bills " ++ kVERSION ++ ", (c) 2015-2019 tobyp <tobyp@tobyp.net>"

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

main :: IO ()
main = do
    (parserState, filenames) <- getArgs >>= billsCliParse
    let bills = parseBill parserState <$> filenames
    transactions <- concat <$> sequence bills
    collapseAccounts $ foldl applyTransaction [] transactions
    exitWith $ ExitSuccess
