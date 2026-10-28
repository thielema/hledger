# Plan: hledger 1.99.6 release (2.0 preview 6)

Target: Sun Nov 1, 2026. Planned Wed Oct 28.
A preview release, following doc/RELEASING.md's release script.

## Scope

Deliverables for a preview release: changelogs, release notes, github release with binaries,
updates to the Install page, manuals snapshot on the site, announcements. No hackage upload.

State on Oct 28:

- 469 commits since 1.99.5, mostly hledger-web (report controls, keys, holdings page, charts)
  and charts (balance/compound/register/holdings `--chart`, SVG, html, `run` pages);
  also arithmetic expressions in posting amounts (#2648), CSV `%{...}` functions, `--infer-imbalance`.
- 7 breaking changes (`!` commits): posting amount expressions; holdings tree account sections;
  `test` command became a `--test` flag; `activity` (two changes); repeated commodity declarations add up;
  alias directives apply before the apply account prefix (#2733).
- Translations: each catalog (de, es, ru, zh-Hans) has about 220 strings to do
  (167 untranslated, 53 missing, 35 stale), mostly new web and chart text.
- CI green on main, nothing unpushed. Open PRs: #2821 (web refactor), #2807 (draft, web refactor).

## Decisions

- Ship the best translations available on release day, including es.po (machine-made, unreviewed).
  We probably won't get complete translations before a release, in general.
- Machine-made catalogs need not be labelled as such; their quality probably
  falls into the same range as human translations'.
- Translations are due by Sat Oct 31, 20:00 UTC (10:00 HST); later ones go into the next release.
- Sunday is kept to the minimum: tag, publish, announce.
- #2648's bounty is settled; mention it in that changelog entry.

## Schedule

Tasks that change translatable text come first, so the message freeze and the request to
translators can go out on Wednesday, giving translators Thursday to Saturday.
Then the riskiest and most time-consuming work.

| Day | Tasks |
|---|---|
| **Wed Oct 28** | #2821/#2807 review/discussion/merge. Single-period bs SVG net mark fix. Message freeze; collect translator addresses; send the translation request. Cache-busting. |
| **Thu Oct 29** | Start `just oldest`. Doc updates: Post-release translation process, Upgrading page, Ledger and Beancount pages. Charts page and screenshots. |
| **Fri Oct 30** | `just generaloptionshelp`, `just manuals`, `just anchortest`. `just relbranch 1.99.6`. Changelogs (`just changelogs`, edit, `just changelogs-finalise` - allow most of a day.) Draft announcements. |
| **Sat Oct 31** | Translation deadline 20:00 UTC. Merge translations as they arrive (on main, cherry-pick to the release branch). `just relnotes` + summary; `doc/ANNOUNCE`; `doc/ghrelnotes`; Install page updates; manuals snapshot (`make -C site snapshot-1.99.6`, site.js / Makefile / caddy). Cherry-pick docs back to main. Last: `just ghbin` (30-40 min), after the translation deadline. |
| **Sun Nov 1** | Check the binaries' `--version`. `just reltags`; push ⚠; `just ghrel`, publish ⚠; site render; announce ⚠. |

## Tasks

In order.

- [ ] **#2821 review/merge** (web refactor). First, since it may touch web text, and so CI can shake it out.
- [ ] **Single-period bs SVG net mark**: the net is a one-point polyline, so invisible;
  it now shows in hledger-web's balance sheet page too. Fix, with a test.
- [ ] **Request new translations**
  - Message freeze: `just i18n-pot`, `just i18n-merge`, `just i18n-check`; commit and push the template and catalogs,
    so translators work from main. Avoid changing translatable text until the release.
  - Collect translator addresses: authors of past catalog work (`git log` on hledger-lib/locale/*.po;
    eg Daniil Iaitskov for ru, Chunhui Ouyang for zh), participants in the i18n PRs/issues (#2735, #2736, #2806),
    and a call for a native Spanish reviewer (es.po is machine-made) and a German translator if none is known,
    on matrix and the forum.
  - Compose and send the mail: per-language gap counts from `just i18n-check`, the deadline (Sat Oct 31, 20:00 UTC),
    a link to TRANSLATING.md, how to send the result back (PR, or the .po file by mail),
    and that partial translations are welcome.
- [ ] **hledger-web static file cache-busting**: serve bootstrap.min.css, hledger.css and hledger.js with
  `?etag=HASH` URLs (wai-app-static `ssUseHash`, computed in `staticFileRoute` / Settings/StaticFiles.hs;
  take care with dev mode, which reads from disk). This release changes hledger.js a lot, and on Oct 27
  Safari kept a stale copy, silently disabling the report keys. Fallback, if it proves risky:
  a "hard-refresh after upgrading" note in the release notes.
- [ ] **Design/document an easy process for users to update and submit translations after release**,
  on all platforms, without building hledger. TRANSLATING.md already covers translating against an installed
  release (user catalogs in the config directory); what's missing is an easy way to get the current template
  and to send the result back. Options to weigh: a "submit" section in TRANSLATING.md (attach the .po to a
  github issue, or mail it); a github issue template for translations; hosted Weblate (free for libre projects;
  web-based, so works everywhere, but another service to maintain). Link it from the release notes and
  announcement.
- [ ] **Review/update affected docs** (site repo)
  - [ ] **Upgrading to hledger 2** (site/src/upgrading.md): the breaking changes, for 1.x users:
    `test` -> `--test`, `activity` output, amounts that may now read as expressions,
    alias vs apply account order, repeated commodity declarations, holdings tree layout.
  - [ ] **Ledger page** (site/src/ledger.md): differences affected by this release, eg alias / apply account
    order now as in Ledger (#2733), amount expressions.
  - [ ] **Beancount page** (site/src/beancount.md): differences affected by this release, eg amount expressions.
  - [ ] **Charts page** (site/src/charts.md): bring up to date with the CLI, html, `run` and hledger-web charts;
    add screenshots (a terminal `--chart`, an SVG chart, a hledger-web report page), reusable in the announcement.
- [ ] **Merge translations** (until Sat Oct 31 20:00 UTC), for each catalog received:
  - put it in hledger-lib/locale/ (or merge the PR); `just i18n-check` (placeholders, stale entries)
  - try it in the CLI (`--lang`), hledger-ui and hledger-web (see the translations skill)
  - commit on main with the translator as author (`git commit --author`); cherry-pick to the release branch
  - credit the translators in the changelog / release notes

## Release process

- [ ] `just oldest` CI passes
- [ ] Options help, manuals, anchor check
- [ ] Release branch
- [ ] Changelogs, with a Breaking changes section and the #2648 bounty mention
- [ ] Release notes and summary (confirm the summary with the maintainer)
- [ ] ANNOUNCE and ANNOUNCE.mastodon (confirm with the maintainer)
- [ ] ghrelnotes, Install page updates
- [ ] Manuals snapshot on the site
- [ ] Binaries (after the translation deadline), tags, push, github release
- [ ] Site render, announcements
