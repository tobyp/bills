import Text.Parsec
import Text.Printf
import Data.Ratio
import Data.Maybe
import GHC.Exts

person = oneOf "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
whitespace = oneOf " \t\v"

amount = do
    prefix <- many1 digit
    char '.'
    suffix <- many1 digit
    return $ (((read prefix) * 100) + (read suffix)) % (100 :: Integer)

comment = do
    char '#'
    many $ noneOf "\n"

entry = do
    debitor <- person
    many1 space
    creditors <- many1 person
    many1 space
    amnt <- amount
    return (debitor, creditors, amnt)

line = do
    skipMany whitespace
    e <- optionMaybe entry
    skipMany whitespace
    optional comment
    char '\n'
    return e

bills = many line

type Person = Char
type Money = Ratio Integer

-- convert "bill lines" (A paid X for B, C and D) into account any number of transactions (A loses/gains a value of X)
itemdeltas :: (Person, [Person], Money) -> [(Person, Money)]
itemdeltas (_, [], _) = error "Invalid item: no creditors"
itemdeltas (d, cs, m) = [(d, -m)]++splitparts cs (m / ((toInteger (length cs)) % (toInteger 1)))
    where splitparts cs mper = [(c, mper) | c <- cs]

-- apply a transaction to a list of accounts: transaction -> accounts_before_transaction -> accounts_after_transaction
applydelta :: (Person, Money) -> [(Person, Money)] -> [(Person, Money)]
applydelta d@(p, m) [] = [d]
applydelta d@(p, m) (a@(ap, am):as)
    | ap == p = (p, m+am):as
    | otherwise = a:(applydelta d as)

-- recursively calculate transfers from the biggest debitor to the biggest creditor until no credits/debits are left.
-- this algorithm only works because we always know there will be at least two accounts (otherwise somebody would owe, but nobody would be owed) and the sum of all credits+debits is 0
collapseAccounts :: [(Person, Money)] -> IO ()
collapseAccounts [] = return ()
collapseAccounts accs = do
    printf "%c pays %f to %c\n" debitor (((fromRational baseline) :: Float)) creditor
    collapseAccounts $ filter (\(p, m) -> m /= 0) $ (creditor, credit + baseline):(debitor, debit - baseline):otherAccs
        where
            ((creditor, credit), (debitor, debit), otherAccs) = minMaxAccounts accs
            baseline = min (-credit) debit

-- separate out the biggest creditor and debitor from an account list
minMaxAccounts :: [(Person, Money)] -> ((Person, Money), (Person, Money), [(Person, Money)])
minMaxAccounts accs = (minAcc, maxAcc, restAccs)
    where
        sortedAccs = sortWith (\(p, m) -> m) accs
        (minAcc:[], restMaxAccs) = splitAt 1 sortedAccs
        (restAccs, maxAcc:[]) = splitAt (length restMaxAccs - 1) restMaxAccs

-- parse the input and collapse the accounts
main :: IO ()
main = do
    content <- getContents
    case parse bills "<input>" content of
        Left err -> do
            putStrLn "Parsing Failed."
            print err
        Right items -> do
            collapseAccounts accounts
                where accounts = (foldr applydelta [] $ concat [itemdeltas i | i <- catMaybes items])