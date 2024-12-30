## import

Import new transactions from one or more data files to the main journal.

```flags
Flags:
     --catchup              just mark all transactions as already imported
     --dry-run              just show the transactions to be imported
```

This command detects new transactions in one or more data files specified as arguments,
and appends them to the main journal. <!-- Existing entries will not be changed. -->

You can import from any input file format hledger supports,
but CSV/SSV/TSV files, downloaded from financial institutions, are the most common import source.

The import destination is the default journal file, or another specified
in the usual way with `$LEDGER_FILE` or `-f/--file`. It should be in journal format.

Examples:

```cli
$ hledger import bank1-checking.csv bank1-savings.csv
```
```cli
$ hledger import *.csv
```

### Import preview

It's useful to preview the import by running first with `--dry-run`,
to sanity check the range of dates being imported,
and to check the effect of your conversion rules if converting from CSV.
Eg:

```cli
$ hledger import bank.csv --dry-run
```

The dry run output is valid journal format, so hledger can re-parse it.
If the output is large, you could show just the uncategorised transactions like so:

```cli
$ hledger import --dry-run bank.csv | hledger -f- -I print unknown
```

You could also run this repeatedly to see the effect of edits to your conversion rules:

```cli
$ watchexec -- 'hledger import --dry-run bank.csv | hledger -f- -I print unknown'
```

Once the conversion and dates look good enough to import to your journal, 
perhaps with some manual fixups to follow, you would do the actual import:

```cli
$ hledger import bank.csv
```

### Overlap detection

Reading CSV files is built in to hledger, and not specific to `import`;
so you could also import by doing `hledger -f bank.csv print >>$LEDGER_FILE`.

But `import` is easier and provides some advantages. 
The main one is that it avoids re-importing transactions it has seen on previous runs. 
This means you don't have to worry about overlapping data in successive downloads of your bank CSV;
just download and `import` as often as you like, and only the new transactions will be imported each time.

We don't call this "deduplication", as it's generally not possible to reliably detect duplicates in bank CSV.
Instead, `import` remembers the latest date processed previously in each CSV file (saving it in a hidden file), and skips any records prior to that date.
This works well for most real-world CSV, where:

1. the data file name is stable (does not change) across imports
2. the item dates are stable across imports
3. the order of same-date items is stable across imports
4. the newest items have the newest dates

(Occasional violations of 2-4 are often harmless; you can reduce the chance of disruption by downloading and importing more often.)

Overlap detection is automatic, and shouldn't require much attention from you, except perhaps at first import (see below).
But here's how it works:

- For each `FILE` being imported from:

  1. hledger reads a file named `.latest.FILE` file in the same directory, if any.
     This file contains the latest record date previously imported from FILE, in YYYY-MM-DD format.
     If multiple records with that date were imported, the date is repeated on N lines.

  2. hledger reads records from FILE.
     If a latest date was found in step 1, any records before that date,
     and the first N records on that date, are skipped.

- After a successful import from all FILEs, without error and without `--dry-run`,
  hledger updates each FILE's `.latest.FILE` for next time.

If this goes wrong, it's relatively easy to repair:

- You'll notice it before import when you preview with `import --dry-run`.
- Or after import when you try to reconcile your hledger account balances with your bank.
- `hledger print -f FILE.csv` will show all recently downloaded transactions. Compare these with your journal. Copy/paste if needed.
- Update your conversion rules and print again, if needed.
- You can manually update or remove the .latest file, or use `import --catchup FILE`.
- Download and import more often, eg twice a week, at least while you are learning.
  It's easier to review and troubleshoot when there are fewer transactions.

<!--
Related: 
[CSV > Working with CSV > Deduplicating, importing](#deduplicating-importing)
-->

### First import

The first time you import from a file, when no corresponding .latest file has been created yet,
all of the records will be imported.

But perhaps you have been entering the data manually, so you know that all of these transactions are already recorded in the journal.
In this case you can run `hledger import --catchup` once.
This will create a .latest file containing the latest CSV record date, so that none of those records will be re-imported.

Or, if you know that some but not all of the transactions are in the journal, you can create the .latest file yourself.
Eg, let's say you previously recorded foobank transactions up to 2024-10-31 in the journal.
Then in the directory where you'll be saving `foobank.csv`, you would create a `.latest.foobank.csv` file containing
```
2024-10-31
```

Or if you had three foobank transactions recorded with that date, you would repeat the date that many times:
```
2024-10-31
2024-10-31
2024-10-31
```

Then `hledger import foobank.csv [--dry-run]` will import only the newer records.

### Importing balance assignments

Journal entries added by import will have all posting amounts made explicit (like `print -x`).

This means that any [balance assignments](https://hledger.org/hledger.html#balance-assignments) in the imported entries would need to be evaluated.
But this generally isn't possible, as the main file's account balances are not visible during import.
So try to avoid generating balance assignments with your CSV rules, or importing from a journal that contains balance assignments.
(Balance assignments are best avoided anyway.)

But if you must use them, eg because your CSV includes only balances:
you can import with [`print`](#print), which leaves implicit amounts implicit.
(`print` can also do overlap detection like import, with the `--new` flag):

```cli
$ hledger print --new -f bank.csv >> $LEDGER_FILE
```

(If you think `import` should preserve implicit balances, please test that and send a pull request.)

### Import and commodity styles

Amounts in entries added by import will be formatted according to the journal's canonical [commodity styles](#commodity-display-style),
as declared by [`commodity` directives](#commodity-directive) or inferred from the journal's amounts.

Related: [CSV > Amount decimal places](#amount-decimal-places).

### Import special cases

If you have a download whose file name varies, you could rename it to a fixed name after each download.
Or you could use a [CSV `source` rule](#source) with a suitable glob pattern,
and import [from the .rules file](#reading-files-specified-by-rule) instead of the data file.

Here's a situation where you would need to run `import` with care:
say you download `bank.csv`, but forget to import it or delete it.
And next month you download it again. This time your web browser may save it as `bank (2).csv`.
So now each of these may have data not included in the other.
And a `source` rule with a glob pattern would match only the most recent file.
So in this case you should import from each one in turn, in the correct order, taking care to use the same filename each time:

```cli
$ hledger import bank.csv
$ mv 'bank (2).csv' bank.csv
$ hledger import bank.csv
```

Here are two kinds of "deduplication" which `import` does not handle
(and generally should not, since these can happen legitimately in financial data):

- Two or more of the new CSV records are identical, and generate identical new journal entries.
- A new CSV record generates a journal entry identical to one(s) already in the journal.

