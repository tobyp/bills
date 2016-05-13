# Bills

Easily divide up bills within a group, so people don't have to pay for stuff they didn't get.

## Format

Comments start with `#` and continue until the end of the line. Empty lines are ignored.

Each line is an entry, containing the following elements separated by arbitrary whitespace:

  * *creditor*: who paid for something (one person)
  * *debitors*: who benefited from this (multiple, possibly including the creditor, and even duplicates for non-even split)
  * *amount*: how much was paid, as a decimal.

## Building

```
ghc bills.hs -o bills
```

## Usage


## Algorithm

The algorithm track a "wallet" for each person : Whenever they pay something, subtract the amount; whenever they receive something, add it.
When `A` pays 1.00 to `B` and `C`, `A` will have 1.00 subtracted, and `B` and `C` will have 0.50 added (because they received that value, e.g. in the form of goods).

Once all transactions are tracked, people with positive balances (who got more than they gave) pay out to people with negative balances, until all balances are zero.

## Example

### Input

```
# Bill 1: Mom-&-Pop Store, 1970-01-01
A	BC	2.00	# A paid 2.00, benefiting B and C
A	A	5.00	# A paid 5.00, but only for themselves
A	ABC	10.00	# A paid 10.00, and whatever was bought got split evenly.
# Bill 2
B	A	2.00
B	AB	3.00
# Cash exchange
C	B	8.88	# C gave B some cash
```

### Output

```
B pays 5.1666665 to A
B pays 4.5466666 to C
```

## Copyright/MIT License

This software can be found at https://github.com/tobyp/bills. It is licensed under the terms of the MIT license, which can be found in the LICENSE file.
