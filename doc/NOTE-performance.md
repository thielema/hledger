# Performance: findings and remaining ideas

Working notes from the 2026-09-23 optimisation session, kept so the measurements, lessons and
the ranked list of remaining ideas survive between sessions. How to benchmark and profile is in
BENCHMARKS.md; the per-version tables and the changelog draft are in _NOTE-performance-2026.md
(untracked); commit-level history is in git (`git log --grep=^perf:`).

Machine: MacBook Pro M5 Pro, GHC 9.14.1, 2026-09 Stackage nightly. Test journal:
examples/100ktxns-1kaccts.journal (8 MB, 100k transactions, 1k accounts, 26 commodities, a third
of the postings carry a unit cost, no assertions or assignments). Numbers are wall-clock seconds
for `hledger balance` unless stated; allocation figures are from `+RTS -s` or `--debug=1` and are
deterministic, wall time varies by a few percent.

## Where the time goes now (main at 16e5246d5, 2026-09-23)

`hledger bal -f examples/100ktxns-1kaccts.journal --debug=1`, 2.83s in total:

| phase | time | allocation | notes |
|---|---|---|---|
| startup + read | 0.05s | 70 MB | |
| parse | 1.71s | 17.1 GB | 60% of the run; ~10% of it is the megaparsec regression (below) |
| journalReverse | 0.09s | 12 MB | a GC pause landing in this slot; the stage itself is trivial |
| journalAddAccountTypes | 0.04s | 161 MB | |
| journalStyleAmounts | 0.23s | 197 MB | rebuilds every posting to set display styles |
| journalTagCostsAndEquityAndMaybeInferCosts | 0.02s | 136 MB | skipped per transaction unless conversion accounts are involved |
| journalBalanceTransactionsAndDeferAssertions | 0.12s | 415 MB | pass 2 skipped (no assertions/assignments) |
| journalInferCommodityStyles | 0.05s | 61 MB | |
| journalInferMarketPricesFromTransactions | 0.07s | 348 MB | timer artifact: only valuation consumes this lazily-built list |
| balance command | 0.38s | 1.2 GB | account tree building, rendering |
| GC, spread across all of the above | 0.75s | | 26% of the run; 19.3 GB allocated, 259 MB max residency, 780 MB RSS |

Other commands on the same journal (main after the first five commits; refresh before quoting):
print 3.6s, register 16.5s (rendering a 26-commodity running balance for 200k lines; real journals
have few commodities), stats 40k txns/s. A lot-using variant (100k journal plus 1000 AAPL buy/sell
pairs with `lots: fifo`, generator tools/_100k-lots-journal.py) takes 3.26s for balance, the extra
0.36s being the lot stages that still touch every transaction or account name.

Versions for context (balance, this machine): 1.25 2.6s, 1.40 4.0s, 1.52 4.1s, 1.99.4 5.7s,
main now 2.83s.

## What was done (11 commits, 5.65s -> 2.83s)

Oldest first, with the gain each gave on the 100k balance run:

- a950e927f skip lot processing when the journal has no lot features: 5.65 -> 4.05s.
- 2255922e8 build postings while parsing: peak residency 300 -> 254 MB, GC -0.15s.
- 947c6132a parse journal items by dispatching on the first character: 4.05 -> 3.30s, parse
  allocation 25.6 -> 17.1 GB.
- 1fa99142b calculate source positions incrementally: -4% (a profile had claimed 8%).
- 3a2907e02 skip the balancer's running-balance pass when nothing needs it: 3.27 -> 3.10s.
- 9c05111a7 `--debug=1` prints each phase's time and allocation (the tool used for everything below).
- 0f4a74d9d infer commodity styles in one pass, inserting only changed styles: 3.15 -> 2.95s.
- 3dd390e41 skip cost/equity tagging without conversion postings: stage 0.075 -> 0.02s.
- 19f772ebf skip lot stages and lot balancing per transaction: lot journal 4.39 -> 3.26s.
- ed7e32d2b balancer: no style inference for exactly-zero sums, no rebuilds when nothing to infer,
  no-op cost conversions: balancing 0.20s/945MB -> 0.17s/690MB.
- 16e5246d5 multiplyQuantities instead of Decimal's (*): balancing -> 0.12s/415MB; also speeds
  -B, valuation and lot arithmetic.

Pending upstream: mrkkrp/megaparsec#612 (filed 2026-09-23), worth ~10% of every command when a
fixed release can be required; the patch (INLINE pragmas on the Stream instances) is on the fork
branch inline-stream-instances, PR to be opened only if the maintainer asks.

## Findings worth remembering

Measurement:

- The `--debug=1` phase timer deep-forces each stage's result, then subtracts the cost of a second
  traversal, so no-op stages read ~0 (they read 0.08-0.12s before that fix). It charges work that a
  normal run leaves unevaluated (inferred market prices) and GC pauses to whichever stage is running
  (journalReverse). Its total matches a normal run within 3%; it costs nothing when off.
- Profiling inflates tiny hot functions (isWideChar, source positions: claimed 8%, real 4%) and
  shifts shares; use it to find candidates, use `--debug=1` and quickbench to judge them.
- Allocation is the reliable signal: it is deterministic and tracks GC cost. Wall time on this
  machine is noisy at the 3-5% level (one quickbench run "showed" a print slowdown that vanished on
  rerun).
- The lot journal is needed for lot-stage work; the plain 100k journal flatters those stages.
  Real journals have assertions, comments, multi-line transactions and few commodities, so run
  `--debug=1` on a real journal before trusting the ranking below.

GC and memory:

- GC time (0.75s) is copying the live journal, not collection overhead: nursery sizes from 4 MB
  (default) to 128 MB change nothing except RSS (tested 2026-09-23: -A16m 2.71s, -A64m 2.93s,
  -A128m 2.81s vs 2.65s). Don't retest.
- Memory-for-time flags: `-xn` (non-moving GC) -10% time for 0.8 -> 1.3 GB RSS; `-xn -F3` -13% for
  +24% RSS; `-F4` -6% for +32% RSS. Judged not attractive for all users.
- Compile flags: package-wide `-fexpose-all-unfoldings -fspecialise-aggressively` gave only -4% for
  a much longer compile. Not adopted. `-O2` alone was not measured separately.
- The journal is ~2.6 KB per transaction in memory (259 MB for 100k). Residency is the remaining
  GC lever: every amount carries its own AmountStyle copy (the styling pass allocates
  `news{asprecision=..}` even when unchanged), account name Texts are not shared between
  postings, and poriginal/ptransaction add pointers per posting.

Parsing:

- Megaparsec >= 9.3.0 (PR 495, ShareInput wrappers) made the plain Text instance delegate through
  a newtype; hledger's parser became ~15% slower and 33% more allocating. Not the wide-char column
  counting added in 9.7.0, which was measured slightly faster than 9.6.1. Bisected by rebuilding
  hledger 1.25 on GHC 9.14 with pinned megaparsec versions (recipe in the performance memory note;
  four small mtl/aeson patches). Current main with megaparsec 9.2.2: balance 2.81 vs 3.13s.
- A failing parse attempt is expensive because megaparsec builds an error value (with sets of
  expected items) per failure; the first-character dispatch commit removed most of those and was the
  single biggest win. Comment and blank-line handling still fails a `try` per line.
- Profiled parser shares after that (exaggerated by profiling): megaparsec plumbing (label ~7%,
  takeWhile_ ~8%, StateT fmap), number parsing (rawnumberp/digitgroupp ~8%, the permutation parser
  in amountp'), date parsing ~8%, comment parsers ~9%, isNonsimpleCommodityChar allocating per
  character (~2% of allocation).
- `postingphelper` building postings strictly during parsing helped; a deepseq there was slower for
  balance because it forces tags and comments that balance never reads.

Finalising:

- The lot stages must run for any journal with lot features even for non-lot reports, because they
  add gain postings and basis-derived amounts that ordinary reports show; `-I` is the opt-out. So lot
  work is gated per transaction (transactionHasLotfulAmounts), not per command.
- journalInferCommodityStyles can't be skipped after the first pass: forecast transactions, auto
  postings and balancing add amounts later.
- Deferring amount styling to report time was rejected: every report would need to do it and get it
  right; unreliable and a lot of code.
- Decimal's (*) goes via Rational, 10^255 and normalizeDecimal (~6 KB and >1 us per multiplication);
  its (/) does the same via fromRational (divideAmount, only on rare paths so far). Upstream
  (Data.Decimal 0.5.2) could be told.

Reports:

- Balance report: accountFromPostings (HashMap alter, PeriodData insert, maPlus per posting) is most
  of its 0.38s; rendering calls showMixedAmountB three times per account (column width computed
  twice plus the render). No big cheap win.
- Register's cost is output volume (running balance rendering), not a bug.

## Remaining ideas, ranked (general ones first)

Expected gains are for the 100k balance run; "general" means every command pays it.

1. Megaparsec regression, ~10%, general. Wait for #612, or ship an interim fix: a newtype around
   the input Text with a direct Stream instance (INLINE pragmas), changing the parser type alias and
   run sites only; about 100 lines, removable later. Lowest risk of anything here.
2. Parser fast paths, 10-15% combined, general, some clarity cost each: (a) the commodity-symbol
   character predicate allocates per character, 1-2%, easy; (b) first-character dispatch for comment
   and blank lines instead of a failing try per line, 3-5%; (c) date parsing and (d) number parsing
   with a common-shape fast path and the general parser as fallback, 3-5% each.
3. Label experiment, unknown, general: nearly every small parser has a `<?>` label, each doing hint
   bookkeeping per call. Stub labels out in a scratch build to measure the ceiling; if several
   percent, keep labels only where they shape user-facing messages.
4. Residency reductions, a few percent, general: share one AmountStyle per commodity (return the
   map's style object when unchanged), intern account names in the parser. Less copying, fewer cache
   misses, less allocation.
5. Styling pass, 8%, general, structural: stop storing display styles per amount and resolve them
   when rendering. Touches everything that shows amounts. Not a quick one.
6. Small finalise stages, ~1% each: account types (plus the regex fallback on every untyped-account
   lookup, accountNameInferType), style inference, cost tagging.
7. Remaining lot overhead on lot journals, ~0.36s: calculateLots still sorts, rebuilds and re-ties
   every transaction (0.135s); basis-from-account-name and transacted-cost inference search every
   account name (0.07s); commodity tags, method coherence.
8. Report side, per command: balance account tree and width computation (~0.1s); print's own
   rendering (~0.8s at 100k, shared by exports, hledger-ui and hledger-web): profile it.
9. Order of magnitude, not incremental: an on-disk cache of the finalised journal keyed by file
   contents and finalising options, skipping 2.3s of 2.83s on unchanged journals; big feature with
   invalidation risks (includes, config, -I and friends, CSV/timeclock inputs). Parallel parsing of
   included files would help multi-file setups only.

Not worth retrying: nursery sizes; memory-doubling GC flags (unless the trade-off is reconsidered);
aggressive specialisation flags; deepseq in postingphelper; a dedicated `--timing` flag (`--debug=1`
was chosen); deferred styling in reports; tying lot checks to `--lots`/holdings.

## Tooling notes

- `stack --profile` on GHC 9.14.1 panics compiling tls; use `stack --stack-yaml stack-prof.yaml
  --work-dir .stack-prof install --library-profiling --executable-profiling --local-bin-path bin
  hledger` (see BENCHMARKS.md). Profiles from this session are archived under doc/profs/202609*.
- `.stack-rtsopts` is a scratch work dir built with `--ghc-options=-rtsopts` for `+RTS` experiments
  (the shipped binary allows `+RTS -s` but not `-A`/`-F`/`-xn`). `stack --work-dir .stack-rtsopts
  path --local-install-root` gives its bin directory.
- `just functest` rebuilds with -Werror, so don't `stack build` in parallel with it.
- quickbench needs the binaries on PATH; `hledger-1.99.4` on PATH is the pre-optimisation reference.
- Scratch stack-mp922.yaml at the repo root builds main against megaparsec 9.2.2 for the regression
  comparison; deletable with .stack-mp922.
