# Bills

Easily divide up bills within a group, so people don't have to pay for stuff they didn't get.

## Format

Comments start with `#` and continue until the end of the line. Empty lines are ignored.

Each line is an entry, containing the following elements separated by arbitrary whitespace:

  * *creditor*: who paid for something (see `Shares`)
  * *debtors*: who benefited from this (see `Shares`)
  * *amount*: how much was paid, as a decimal.

### Shares

Sometimes, multiple people spend money, and/or the money is spent for multiple people, and not necessarily at identical rates.
For this, shares can be specified.
A share is either:
 - A person (e.g. `A` or `Bob`, starting with exactly one uppercase letter and continuing with any number of lowercase letters)
 - A group of people (optinally weighted, optionally comma separated) (e.g. `AB`, `Alice,Bob`)

Groups can be weighted to by parenthesizing them: `1(AB)2(BC)`. The amount will first be distributed to groups according to group weight, and then inside the groups according to the weights inside the parentheses.

Some examples:


 - `A`, `Bob`: a single share for one person
 - `1A2B`, `3A,Bob,2C`: simple shares for multiple people (default is one share, so Bob gets a sixth of that item)
 - `50A50B`: half and half, for those who like to think in percentages (note: the software will not check that they actually sum to 100)
 - `(AB)(CDE)`: quarters for each of A and B, and sixths for each of C, D and E
 - `3(1A,2B)(DE)`: quarter for A, half for B, and eight for D and E each
 - `7(3(AB)5(3D7E))(8F1(3G1H))`  you're insane

## Building

```
ghc bills.hs -o bills
```

## Usage

```
bills < example.txt
```

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

# Bill 2: Hot Dog Stand, 1970-01-02
B	A	2.00
B	AB	3.00

# Cash exchange
C	B	8.88	# C gave B some cash

# Bill 3: Tickets, Stadium, 1970-01-05
A,B		AB,2C		# A and B each paid half of the tickets, which included one for each of them and 2 for C
```

### Output

```
C pays 16.166666 to A
C pays 1.2866666 to B
```

## Wishlist

 - Different modes besides "settlements"
 	- Transactions per person ("X got #.## from Y", "X paid #.## for Z")
 	- Total amounts paid/received ("X paid ###.## and received ###.# in total")
 	- Statistics
 	- Charts and Diagrams (dot/graphviz, tikz, R)
 - Alias system, possibly scoped (e.g. if couples share an account)

## Copyright/MIT License

This software can be found at https://github.com/tobyp/bills. It is licensed under the terms of the MIT license, which can be found in the LICENSE file.
