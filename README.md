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
 - A group of people (optinally weighted, optionally comma separated) (e.g. `AB`, `A,B`, `AliceBob`, `Alice,Bob`)

Groups can be weighted to by parenthesizing them: `1(AB)2(BC)`. The amount will first be distributed to groups according to group weight, and then inside the groups according to the weights inside the parentheses.

Some examples:


 - `A`, `Bob`: a single share for one person
 - `1A2B`, `3A,Bob,2C`: simple shares for multiple people (default is one share, so Bob gets a sixth of that item)
 - `50A50B`: half and half, for those who like to think in percentages (note: the software will not check that they actually sum to 100)
 - `(AB)(CDE)`: quarters for each of A and B, and sixths for each of C, D and E
 - `3(1A,2B)(DE)`: quarter for A, half for B, and eight for D and E each
 - `7(3(AB)5(3D7E))(8F1(3G1H))`  you're insane

### Exclusions

With group-style defines, especially with large groups, it may be convenient to exclude certain members of the group for specific entries, without having to redefine the entire (possible complex) group structure.
This can be achieved with exclusion syntax, e.g. `ABCD\A`, which is equivalent to `ABC`.
The `\\` operator binds less tightly than union, but respects parentheses, e.g. `(AB)C\D` is the same as `ABC\D`, and `ABC\(AB\A)` equals `AC`.
Exclusion disregards weights, and will remove all shares on the left side for any person on the right side, e.g. `5A3B2C6D\1A2B` is equal to `2C6D`.

### Defines

The `%define` and `%undefine` syntax can be used to re-define person names.
This has two main uses (though other, more creative uses, are of course possible):
 - Creating "groups" by defining a new Person, e.g. `%define E ABCD` and then using the shorthand `E` for items shared equally by those four parties. Groups can be any weighted expression, as described above.
 - Having others pay for a specific person's expenses, e.g. `%define X AB` for `A` and `B` to share all expenses `X` incurs. This is nice when you want to "treat" someone to something, and foot their bill.

Defines will remain in effect until they are removed by an `%undefine` line, and are applied both for creditors and debitors.

### Variables

Similar to defines providing shorthands for people/group shares, variables can be used as shorthands for money values.
They can be created and modified using the `%set` directive, and removed with `%unset`.
Variable names may begin with any letter or and underscore, and may continue with any letter, underscore, or digit.
If a variable is not set (or was unset), using it is an error.

### Currencies

To work with multiple currencies, it is suggested to define variables for their currency symbols, and set them to the cost of that currency in the implicit "base" currency.
For example, when usually working in a "base currency" of EUR, one might create a variable `GBP` and set it to `1.09`, i.e. one GBP costs 1.09 EUR.
Then an item that cost 5 GBP would be entered as costing `5*GBP`.

## Building

```
ghc bills.hs -o bills
```

## Usage

```
bills example.txt
```

## Algorithm

The algorithm track a "wallet" for each person : Whenever they pay something, subtract the amount; whenever they receive something, add it.
When `A` pays 1.00 to `B` and `C`, `A` will have 1.00 subtracted, and `B` and `C` will have 0.50 added (because they received that value, e.g. in the form of goods).

Once all transactions are tracked, people with positive balances (who got more than they gave) pay out to people with negative balances, until all balances are zero.

## Example

### Input

```
# Bill 1: Mom-&-Pop Store, 1970-01-01
A	BC.	2.00	# A paid 2.00, benefiting B and C
A	A.	5.00	# A paid 5.00, but only for themselves
A	ABC.	10.00	# A paid 10.00, and whatever was bought got split evenly.

# Bill 2: Hot Dog Stand, 1970-01-02
B	A.	2.00
B	AB.	3.00

# Cash exchange
C	B.	8.88	# C gave B some cash

# Bill 3: Tickets, Stadium, 1970-01-05
A,B		AB,2C.		# A and B each paid half of the tickets, which included one for each of them and 2 for C
```

### Output

```
todo: C pays 16.166666 to A
todo: C pays 1.2866666 to B
```

### Additional Options

Additionally to the "todos" that describe how to balance wallets after a set of transactions, `bills` can also calculate flows, which show for each user how much they have spent/received.
To do this, specify the `-F` option, and receive the following output (in addition to the todos, which are not repeated here):

```
flow: A consumed 22.833334 and is owed 16.166666 (so, is out 39.0)
flow: B consumed 25.713333 and is owed 1.2866666 (so, is out 27.0)
flow: C consumed 26.333334 and owes 17.453333 (so, is out 8.88)
```

As an explanation:

- "consumed" is usually to be interpreted as "goods in the value of".
- "is out" means from your wallet, and includes the money that was spent on behalf of someone else.
- "owes"/"is owed" shows whether you need to give or receive money in order to "square" with everyone else.

## Wishlist

 - Different modes besides "settlements"
 	- Transactions per person ("X got #.## from Y", "X paid #.## for Z")
 	- Total amounts paid/received ("X paid ###.## and received ###.# in total")
 	- Statistics
 	- Charts and Diagrams (dot/graphviz, tikz, R)
 - Alias system, possibly scoped (e.g. if couples share an account)

## Copyright/MIT License

This software can be found at https://github.com/tobyp/bills. It is licensed under the terms of the MIT license, which can be found in the LICENSE file.
