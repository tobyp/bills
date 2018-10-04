import Text.Parsec
import Text.Printf
import Data.Ratio
import Data.Maybe
import GHC.Exts
import Control.Monad (void)

type Person = String
type Money = Ratio Integer
type Weight = Ratio Integer

data Share = Share Person Weight deriving Show
data Account = Account Person Money deriving Show
data Transaction = Transaction Person Money deriving Show

large = oneOf "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
small = oneOf "_abcdefghijklmnopqrstuvwxyz"
whitespace = oneOf " \t\v"
decimalSeparator = oneOf ".,"

person = do
    a <- large
    b <- many small
    return (a:b)

number = do
    intpart <- many1 digit
    fracpart <- option (0 % 1) $ do {
        decimalSeparator;
        fracpart <- many1 digit;
        return $ (read fracpart) % (10 ^ (length fracpart)) }
    return $ (read intpart) % 1 + fracpart

share_person = do
    who <- person
    return [Share who (1 % 1)]

share_paren = do
    char '('
    shs <- shares
    char ')'
    return shs

share = do
    weight <- option (1 % 1) number
    shs <- try share_person <|> share_paren
    return $ scaleShare  (1 / weight) <$> shs

shares = do
    shs <- sepBy1 share (optional $ char ',')
    return $ normalizeShares $ concat shs

comment = do
    char '#'
    many $ noneOf "\n"

entry = do
    creditors <- shares
    many1 space
    debtors <- shares
    many1 space
    amount <- number
    return $ shareTransaction creditors amount debtors

line = do
    skipMany whitespace
    e <- optionMaybe entry
    skipMany whitespace
    optional comment
    return e

eol = (try $ string "\n\r") <|> (try $ string "\r\n") <|> (try $ string "\n")

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

main :: IO ()
main = do
    content <- getContents
    case parse bills "<input>" content of
        Left err -> do
            putStrLn "Parsing Failed."
            print err
        Right transactions -> do
            collapseAccounts $ foldl applyTransaction [] transactions
