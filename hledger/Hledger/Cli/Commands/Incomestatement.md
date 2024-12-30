## incomestatement

(is)

Show revenue inflows and expense outflows during the report period.
Amounts are shown with normal positive sign, as in conventional financial statements.

```flags
Flags:
     --sum                  show sum of posting amounts (default)
     --valuechange          show total change of period-end historical
                            balance value (caused by deposits, withdrawals,
                            market price fluctuations)
     --gain                 show unrealised capital gain/loss (historical
                            balance value minus cost basis)
     --count                show the count of postings
     --change               accumulate amounts from column start to column
                            end (in multicolumn reports) (default)
     --cumulative           accumulate amounts from report start (specified
                            by e.g. -b/--begin) to column end
  -H --historical           accumulate amounts from journal start to column
                            end (includes postings before report start date)
  -l --flat                 show accounts as a flat list (default). Amounts
                            exclude subaccount amounts, except where the
                            account is depth-clipped.
  -t --tree                 show accounts as a tree. Amounts include
                            subaccount amounts.
     --drop=N               flat mode: omit N leading account name parts
     --declared             include non-parent declared accounts (best used
                            with -E)
  -A --average              show a row average column (in multicolumn
                            reports)
  -T --row-total            show a row total column (in multicolumn reports)
     --summary-only         display only row summaries (e.g. row total,
                            average) (in multicolumn reports)
  -N --no-total             omit the final total row
     --no-elide             don't squash boring parent accounts (in tree
                            mode)
     --format=FORMATSTR     use this custom line format (in simple reports)
  -S --sort-amount          sort by amount instead of account code/name
  -% --percent              express values in percentage of each column's
                            total
     --layout=ARG           how to show multi-commodity amounts:
                            'wide[,WIDTH]': all commodities on one line
                            'tall'        : each commodity on a new line
                            'bare'        : bare numbers, symbols in a column
     --base-url=URLPREFIX   in html output, generate hyperlinks to
                            hledger-web, with this prefix. (Usually the base
                            url shown by hledger-web; can also be relative.)
  -O --output-format=FMT    select the output format. Supported formats:
                            txt, html, csv, tsv, json.
  -o --output-file=FILE     write output to FILE. A file extension matching
                            one of the above formats selects that format.
```

This command displays an [income statement](http://en.wikipedia.org/wiki/Income_statement), 
showing revenues and expenses during one or more periods. 

It shows accounts declared with the `Revenue` or `Expense` type
(see [account types](https://hledger.org/hledger.html#account-types)).
Or if no such accounts are declared, it shows top-level accounts named
`revenue` or `income` or `expense` (case insensitive, plurals allowed) and their subaccounts.

Example:
```cli
$ hledger incomestatement
Income Statement 2008

                   || 2008 
===================++======
 Revenues          ||      
-------------------++------
 income:gifts      ||   $1 
 income:salary     ||   $1 
-------------------++------
                   ||   $2 
===================++======
 Expenses          ||      
-------------------++------
 expenses:food     ||   $1 
 expenses:supplies ||   $1 
-------------------++------
                   ||   $2 
===================++======
 Net:              ||    0 
```

This command is a higher-level variant of the [`balance`](#balance) command,
and supports many of that command's features, such as multi-period reports.
It is similar to `hledger balance '(revenues|income)' expenses`,
but with smarter account detection, and revenues/income displayed with
their sign flipped.

This command also supports the
[output destination](hledger.html#output-destination) and
[output format](hledger.html#output-format) options
The output formats supported are
`txt`, `csv`, `tsv` (*Added in 1.32*), `html`, and `json`.
