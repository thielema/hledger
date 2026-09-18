# SPEC: print command

Notes on some of print's behaviour.

## Effects of certain output flags

### No output flags (default output)

By default, print tries to show each entry as it is written in the journal file,
except for alignment. And it shows entries in date-then-parse order.
Comment lines immediately preceding an entry (with no blank line between) are part of it
(`tprecedingcomment`) and are shown before it, verbatim.

### `--export`

Reproduces the journal file(s) rather than a date-sorted list of entries.
The journal reader records every top-level item verbatim and in order in `jitems`
(see `JournalItem` in Types.hs); export walks those items, emitting directives,
comment lines, comment blocks and blank lines as written, and replacing each transaction
placeholder with print's normal rendering of that transaction (looked up in `jtxns` by
source position, so filtering and processing of `jtxns` are respected). Details:

- `include` lines are dropped; the included file's items follow inline.
  (A future `--export=file` mode could keep the include line and omit them.)
- `apply account`/`alias` and their `end` forms are dropped: their effect is baked into
  the stored account names. Hence an `account` declaration inside an `apply account` block
  is reproduced without its prefix.
- `!`/`@` prefixes and the Ledger-only directives hledger ignores are dropped.
- Transactions tagged `_generated-transaction` and postings tagged `_generated-posting` are dropped,
  so `--forecast`, `--auto`, `--infer-equity` have no effect.
- Transactions with no placeholder (from non-journal files, eg an included CSV) are appended at the end.
- A `decimal-mark`, `D` or `Y` directive inside an included file, once inlined, also affects
  later entries of the parent file. Known limitation.
- txt output format only.

### `--round`

Controls rounding/padding of displayed amounts:

- `none` — show original decimal digits, as in the journal (default)
- `soft` — add or remove trailing decimal zeros to match commodity precision
- `hard` — round posting amounts to commodity precision (can unbalance transactions)
- `all` — also round cost amounts to commodity precision (can unbalance transactions)

### `--verbose-tags`

Makes certain normally-hidden tags visible (in comments):

- `ptype: acquire/dispose/transfer-from/transfer-to` — lot posting classification
- `cost-tagged:` — marks postings that have or were given a transaction cost
- `conversion-tagged:` — marks equity conversion postings
- `generated-posting:` — marks auto-generated postings (from transaction modifiers or --infer-equity)
- `modified-transaction:` — marks transactions modified by auto posting rules
- `generated-transaction: <period>` — marks forecast transactions from periodic rules

Without this flag, these tags still exist internally (queryable) but don't appear in print output.

### `-x` / `--explicit`

Shows all inferred balancing amounts and balancing costs:

- Inferred amounts are shown
- Inferred costs are shown
- Balance assignment amounts are shown explicitly

### `--lots`

Triggers lot calculation, which restructures postings:

- Cost basis fields are made explicit, with missing parts filled in
- Lots acquired on the same day get uniquifying labels added if needed
- All lot postings get specific lot subaccounts added (e.g. `assets:stocks` → `assets:stocks:{2026-01-15, $50}`)
- Transfer postings and dispose postings affecting multiple lots are split into one per lot
