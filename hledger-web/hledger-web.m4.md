% hledger-web(1)
% _author_
% _monthyear_

_notinfo_({{
# NAME
}})

hledger-web - web interface and API for `hledger`, a robust, friendly plain text accounting app.

_notinfo_({{
# SYNOPSIS
}})

`hledger-web [OPTS] [QUERY]`\
or\
`hledger web [OPTS] [QUERY]`

_notinfo_({{
# DESCRIPTION
}})

This manual is for hledger's web interface, version _version_.
See also the hledger manual for common concepts and file formats.

_hledgerdescription_

_web_({{
<div class="screenshots-right">
<a href="/images/hledger-web/normal/register.png" class="highslide" onclick="return hs.expand(this)"><img src="/images/hledger-web/normal/register.png" title="Account register view with accounts sidebar" height="180" /></a>
<a href="/images/hledger-web/normal/journal.png" class="highslide" onclick="return hs.expand(this)"><img src="/images/hledger-web/normal/journal.png" title="Journal view" height="180" /></a>
<a href="/images/hledger-web/normal/help.png" class="highslide" onclick="return hs.expand(this)"><img src="/images/hledger-web/normal/help.png" title="Help dialog" height="180" /></a>
<a href="/images/hledger-web/normal/add.png" class="highslide" onclick="return hs.expand(this)"><img src="/images/hledger-web/normal/add.png" title="Add form" height="180" /></a>
</div>
}})

hledger-web is a simple web application for browsing and adding transactions.
It provides a more user-friendly UI than the hledger CLI or
hledger-ui TUI, showing more at once (accounts, the current account register,
balance charts) and allowing history-aware data entry, interactive searching,
and bookmarking.

hledger-web also lets you share a journal with multiple users, or even the public web.
There is no user authentication, so if you need that you should put it
behind a suitable web proxy.  As a small protection against data loss
when running an unprotected instance, it writes a numbered backup of
the main journal file (only) on every edit.

Like hledger, it _inputfileswithptr_

hledger-web can be run in three modes:

- `--serve-browse` mode (the default):
  the app serves the web UI and JSON API,
  and opens your default web browser to show the app if possible,
  and exits automatically once no browser window has shown it for fifteen minutes.

- `--serve`: the app just serves the web UI and JSON API.

- `--serve-api`: the app just serves the JSON API.

In all cases hledger-web runs as a foreground process, logging requests to stdout.

# OPTIONS

hledger-web provides the following options:

```
Flags:
     --serve --server       serve and log requests, don't browse or auto-exit
     --serve-api            like --serve, but serve only the JSON web API,
                            not the web UI
     --allow=view|add|edit  set the user's access level for changing data
                            (default: `add` on a local-only address, `view`
                            otherwise). It also accepts `sandstorm` for that
                            platform (reads from the `X-Sandstorm-Permissions`
                            request header).
     --cors=ORIGIN          allow cross-origin requests from the specified
                            origin; setting ORIGIN to "*" allows requests from
                            any origin
     --host=IPADDR          listen on this IP address (default: 127.0.0.1)
     --port=PORT            listen on this TCP port (default: 5000); 0 means
                            a free port chosen by the OS
     --socket=SOCKET        listen on the given unix socket instead of an IP
                            address and port (unix only; implies --serve)
     --base-url=BASEURL     set the base url (default: http://IPADDR:PORT)
     --lang=LANG            show the web UI in this language (en, de, zh); or
                            with auto, use each viewer's browser language if
                            available, otherwise the server's. (Default: the
                            browser's language if available, otherwise en.)
```

By default hledger-web listens only on IP address `127.0.0.1`,
which can be accessed only from the local machine.

To allow access from elsewhere, use `--host` to specify an externally accessible address configured on this machine.
The special address `0.0.0.0` causes it to listen on all of this machine's addresses.

Similarly, you can use `--port` to listen on a TCP port other than 5000.
This is useful if you want to run multiple hledger-web instances on a machine.
`--port 0` makes the operating system choose a free port, which is reported
in the startup message and in the default base url, and is where the browser
is opened in `--serve-browse` mode. This can be useful eg when scripting.

When `--socket` is used, hledger-web creates and communicates via a socket file instead of a TCP port.
This can be more secure, respects unix file permissions, and makes certain use cases easier,
such as running per-user instances behind an nginx reverse proxy. (Eg:
`proxy_pass http://unix:/tmp/hledger/${remote_user}.socket;`.)

You can use `--base-url` to change the protocol, hostname, port and path that appear in
hledger-web's hyperlinks. This is useful eg when integrating hledger-web within a larger website.
The default is `http://HOST:PORT/` using the server's configured host address and TCP port
(or `http://HOST` if PORT is 80).
Note this affects url generation but not route parsing.

<!--  #2139
`--file-url=URL`
: set a different base url for static files (default: `BASEURL/static/`)

hledger-web normally serves static files itself,
but if you wanted to serve them from another server,
eg for better caching or cookie-less serving on high performance websites,
you can customise their urls with this.
-->

hledger-web also supports many of hledger's [general options](hledger.md#options):

_webgeneraloptions_

hledger-web shows accounts with zero balances by default (like `hledger-ui`, and unlike `hledger`).
Using the `-E/--empty` flag will reverse this behaviour.
If you see accounts which appear to have a zero balance, but cannot be hidden with `-E`,
it's because they have a mixed-cost balance, which looks like zero when costs are hidden.
(hledger-web does not show costs.)

Report options and query arguments given at startup become url state:
hledger-web translates them into the url that browse mode opens, and
that `--serve` prints, eg `hledger-web -V -M` opens
`/journal?interval=monthly&value=end`, and `hledger-web --cleared -3 assets`
opens `/journal?q=assets status:*&depth=3` (url-encoded). The server
itself keeps the defaults, so every view is fully described by its
url: a bare url shows the default view, and editing the url (or the
search and the links above each page) changes the view. Report options
the web UI has no equivalent for are ignored; options that map to
page-local state, like `--historical`, shape the opening page only.

This means a startup query is a starting search, not a restriction: a
viewer can edit or clear it, and the JSON API serves the whole journal
regardless. To actually restrict what hledger-web serves, give it a
file holding only the entries to serve (eg one written by `hledger
print QUERY`).

If you use the bash shell, you can auto-complete flags by pressing TAB in the command line.
If this is not working see [Install > Shell completions](install.html#shell-completions).

# WEB UI

hledger-web's main views are:

- the **journal view** (the home page), showing journal entries, newest first, with their postings;
- the **register view**, showing the transactions affecting one account (and its subaccounts),
  with a running balance and a balance chart, the account named by an `account` parameter,
  eg `/register?account=assets:bank` (with `subs=0`, excluding its subaccounts), which the
  register's links and search keep; and
- the **report views**, described [below](#reports): the balance sheet, income statement,
  cashflow statement (and holdings, for journals using lots), and All accounts report,
  all linked from the sidebar.

The register's chart shows the running balance over the page's transactions, across the page.
With several commodities, a legend (or the `[` and `]` keys) chooses the one charted, each on its own scale,
starting with the one with the most transactions.
Clicking a point goes to its transaction, and dragging across the chart
shows the transactions of the dates dragged across.

A row of links at the top of every page leads to the journal and report views.
Below it are the search form, the options, and then the view's heading and content.
The options are hidden until opened (click them, or press `o`); while hidden, they list
the ones in effect, eg "Monthly · Depth 2 · Tree", so a view's shape is always visible.

The journal and register views show the newest 1000 matching transactions, with links to the older
and newer pages and to pages by number, so that a page stays a reasonable size
however large the journal is (a `page` parameter in the URL names the page).
When the matching transactions span more than one year, a row of years among the
controls links to the view narrowed to each year; "All" widens it again, and shift-clicking
a year selects a range of years (a `date:FIRST..END` search term, whose years the row marks).
When they span more than twenty years, the years are shown one row per decade.

The **sidebar** lists accounts and their balances (parent balances include subaccounts,
and multiple commodities are shown one above the other).
When a report shows its accounts as a tree (the Accounts row), the sidebar does too,
with carets that fold and open each group in place. Folds are remembered in the browser,
as choices laid over each page's baseline: a page with a depth limit starts folded
to it, one without starts fully open, and either way the groups you have opened or
closed by hand stay as you left them. A caret clicked back to its page's baseline
is forgotten.
Click an account name to see its register, or an amount to see the transactions it is
made of. In the journal and register views, click a date to see that day's journal entries.

The **search form** filters every view with hledger's [query](hledger.md#queries) syntax,
eg `expenses date:thismonth`; the help dialog summarises the query types.
The current view and search are reflected in the URL, so views can be bookmarked and shared.

The **add form** (press `a`, or click "Add a transaction" in the journal view)
adds a transaction to the main journal file, if the [access level](#permissions) allows it.
It autocompletes account names and descriptions from your existing entries.

The **help dialog** (press `?`, or click the question mark button) lists these keyboard shortcuts,
which work when you are not typing in a field:

- `h` or `?` - show or hide the help dialog
- `g` - go to another page: shows a menu of the pages, each with its key;
  then `j` journal, `b` balance sheet, `i` income statement, `c` cashflow statement,
  `h` holdings (for journals using lots), `a` all accounts,
  or `n` / `p` the next / previous page in that order, wrapping round (escape to cancel)
- `a` or `n` - add a transaction (escape to cancel)
- `s` - show or hide the sidebar
- `o` - show or hide the report options
- `c` - show or hide the report's chart (on pages with one)
- `[` / `]` - chart the previous / next commodity (on pages whose chart has several, wrapping round)
- `d` - show or hide the report's data, its table (on pages with a chart)
- `f` - full window: show only the report's heading and data, or everything (`s`, `o` and `/` leave it, showing what they ask for)
- `z` - show or hide accounts with zero balances
- `t` - show accounts as a tree, or a list
- `H` - show ending balances, or balance changes
- `L` - show or hide lot detail
- `V` - show amounts at market value, or as recorded
- `B` - show amounts at cost, or as recorded
- `R` - show only real postings, or all
- `U`, `P`, `C` - show only unmarked, pending, or cleared transactions, or all
- `S` - sort by amount, or by account
- `T`, `A` - add or remove the row total and average columns (multi-period reports)
- `D`, `W`, `M`, `Q`, `Y` - set the report interval (daily, weekly, monthly, quarterly, yearly); pressed again, remove it
- `0`-`9`, `-`, `+` - set or adjust the account depth limit (`-` and `+` step between 0 and the deepest)
- `/` - focus the search form, ready to add a term (escape to leave it)

The pages are light or dark, following your OS or browser's color scheme setting,
and switch when it changes.

Editing, uploading and downloading journal files is described [below](#editing-uploading-downloading).

# PERMISSIONS

You can restrict who can access hledger-web by

- setting the IP address it listens on (see `--host` above).
  By default it listens on 127.0.0.1, accessible to users on the local machine only.
- or by putting it behind an authenticating proxy, such as caddy or apache
- or by putting it behind a firewall.

And you can restrict what the users reaching it can do,
by specifying the `--allow=ACCESSLEVEL` option at startup.
ACCESSLEVEL is one of:

- `view` - allows viewing the journal file(s)
- `add`  - also allows adding new transactions to the main journal file
- `edit` - also allows editing, uploading or downloading the journal file(s)
- `sandstorm` - (for the hledger-web Sandstorm app:) allows whichever of `view`, `add`, or `edit` are specified in the `X-Sandstorm-Permissions` HTTP header

The default access level is `add` when listening on a local-only address
(`127.0.0.1`, `::1`, `localhost`, or a unix socket), and `view` otherwise.
To allow more than the default access, start it with an explicit `--allow=add` or `--allow=edit` option.

# EDITING, UPLOADING, DOWNLOADING

If you start hledger-web with `--allow=edit`,
you'll see a new "spanner" button to the right of the search form.
Clicking this will let you edit, upload, or download the journal
file or any files it includes.

Note, unlike any other hledger command, in this mode you (or any visitor)
can alter or wipe the data files.
Normally when hledger-web changes any data, it will save a numbered backup of the file
(assuming file permissions allow it, the disk is not full, etc.)
hledger-web is not aware of version control systems, currently; if you use one,
you'll have to arrange to commit the changes yourself.

Changes which would leave the journal file(s) unparseable or invalid
(eg with failing balance assertions) are prevented.

# REPORTS

Besides the journal and the account registers, hledger-web shows these
reports. The sidebar and the links at the top of every page lead to each
of them, and so does the `g` key followed by the first letter of the report's name:

- **Balance sheet** (`b`, `/balancesheet`): assets, liabilities and net worth, like the
  [balancesheet](hledger.md#balancesheet) command.
  Its Equity option adds an equity section, like [balancesheetequity](hledger.md#balancesheetequity).
- **Income statement** (`i`, `/incomestatement`): revenues, expenses and net income, like
  [incomestatement](hledger.md#incomestatement).
- **Cashflow** (`c`, `/cashflow`): changes in cash accounts, like [cashflow](hledger.md#cashflow).
- **Holdings** (`h`, `/holdings`): investments and their performance, like [holdings](hledger.md#holdings).
  Shown only for journals that use [lots](hledger.md#lots-and-capital-gains); see [below](#holdings).
- **All accounts** (`a`, `/balance`): all accounts, or those you search for, like [balance](hledger.md#balance).

Each report shows a chart and then a table (by default; both can be toggled).
Click any amount to see the transactions it is made of.

## Options

A report's options (click Options, or press `o`) change what it shows.
These are similar to hledger's command-line [options](hledger.md#options) 
and [hledger-ui's keys](hledger-ui.md#keys).
The main ones, with their keyboard shortcuts:

- **Interval** (`D`, `W`, `M`, `Q`, `Y`): one column for the whole period,
  or one per day, week, month, quarter or year
  (see [Report intervals](hledger.md#report-intervals)).
  Click a column's heading to see the report for just its dates.
- **Show** (`H`): balance changes in each period, or ending balances
  (which include everything before the period).
  The balance sheet shows ending balances by default and the other reports show changes,
  as their commands do; the report's heading says which it is showing.
- **Depth** (`0`-`9`, `-`, `+`): how many levels of accounts to show
  (see [Depth](hledger.md#depth)). Depth 0 shows only the totals.
- **Accounts** (`t`): a list, or a tree with subtotals. In a tree, the accounts
  below the depth limit are folded, and their carets open them in place.
- **Amounts** (`B`, `V`): as recorded, at cost, or at market value at the end of each period
  (see [Cost reporting](hledger.md#cost-reporting) and [Value reporting](hledger.md#value-reporting)).
- **Zero balances** (`z`): hide or show accounts whose balance is zero.
- **Equity** (balance sheet only): hide or show the equity section.
  The browser remembers this choice for each hledger-web server.
- **Status** (`U`, `P`, `C`), **Postings** (`R`), account types, and commodities:
  show only transactions with that [status](hledger.md#status), only real postings,
  or only some [account types](hledger.md#account-types) or commodities.
  These rows edit the search, adding a term like `status:*` or `type:A`.
  Shift-click selects several types or commodities.

The options you choose stay in effect as you move to other pages, except Show,
which each report sets to its own default.
On the journal and register, the report options are shown muted, but are kept.

The search form filters reports as it does the other pages, and its `date:` terms set the report's dates.
An interval or depth typed in the search, like `date:monthly` or `depth:2`,
moves into the options when the page reloads.
A depth limit for some accounts only, like `depth:assets=3`, stays in the search.

(For more detail, see the hledger manual, eg [CONCEPTS](hledger.md#part-4-concepts).)

## Charts

A report's chart is the one its command draws with [`--chart`](hledger.md#charts):
eg a bar per account, or with an interval, each account over time,
and for the statements, each section and the net total.
In tree mode it shows the accounts down to the depth limit.
With several commodities, use the legend, or the `[` and `]` keys, to choose the one charted.

The `c` key hides or shows the chart, and `d` the table.
The browser remembers this for each hledger-web server.

## From a report to its transactions

Click an amount to see its account's register for that period:
the transactions the amount is made of.
An ending balance's register starts with the balance brought forward,
so its final running balance is that amount.
A section's total shows the register of all the section's accounts.
An amount shown at cost or market value leads to a register showing its amounts the same way.

Some things can make the register look different from the report:

- The statements show liabilities, equity and revenues as positive amounts,
  as their commands do, so their registers show the same amounts negated.
  The amount's tooltip says so.
- When a posting has a date of its own, the report counts it on that date,
  but the register lists its whole transaction, on the transaction's date.

## Holdings

The holdings report shows one row for each commodity held in each account,
with its cost basis, value and gains. Its chart has a bar for each one,
showing its cost basis and unrealised gain or loss.
Its own options can show:

- each lot separately (**Lots**, `L`),
- fully disposed positions and their realised gains (**Closed positions**),
- all values in the market valuation commodity (**Values**).

Its depth limit doesn't fold accounts: as on the command line, it merges
the deeper accounts' positions, by commodity, into the accounts at the limit.

On journals that use lots, every page also has a Lots option, showing
lot subaccounts and per-lot postings.

## URL parameters

A page's address includes its search and options, so a bookmark or a shared link
shows the same view. These are the parameters, mostly corresponding to hledger options:

- `q` - the search
- `interval` - `daily`, `weekly`, `monthly`, `quarterly` or `yearly` (like `-D`, `-W`, `-M`, `-Q`, `-Y`)
- `depth` - a number (like `--depth`)
- `accum` - `change`, `historical` or `cumulative` (like `--change`, `-H`, `--cumulative`)
- `value` - `none`, `cost`, or `then`, `end`, `now` or a date, optionally followed by `,COMMODITY` (like `--value`)
- `empty` - `1` to show zero balances, `0` to hide them (like `-E`)
- `list` - `tree` or `flat` (like `--tree`, `--flat`)
- `sort` - `amount` (like `-S`)
- `total`, `avg`, `pct` - `1` to add totals, averages, or show percentages (like `-T`, `-A`, `-%`)
- `equity` - `1` or `0`, on the balance sheet (the old `/balancesheetequity` address leads to `equity=1`)
- `lots` - `1` to show lots (like `--lots`)
- `closed` - `1`, on the holdings page, to show closed positions

# RELOADING

hledger-web detects changes made to the files by other means (eg if you edit
it directly, outside of hledger-web), and it will show the new data
when you reload the page or navigate to a new page.
If a change makes a file unparseable,
hledger-web will display an error message until the file has been fixed.

(If you are viewing files mounted from another machine, make sure that
both machines have roughly the same idea of what time it is.)

# JSON API

In addition to the web UI, hledger-web also serves a JSON API that can be
used to get data or add new transactions.
If you want the JSON API only, you can use the `--serve-api` flag. Eg:

```cli
$ hledger-web -f examples/sample.journal --serve-api
...
```

You can get JSON data from these routes:

```
/version
/accountnames
/transactions
/prices
/commodities
/accounts
/accounttransactions/ACCOUNTNAME
/openapi.json
```

Eg, all account names in the journal (similar to the [accounts](hledger.md#accounts) command).
(hledger-web's JSON does not include newlines, here we use python to prettify it):

```cli
$ curl -s http://127.0.0.1:5000/accountnames | python3 -m json.tool
[
    "assets",
    "assets:bank",
    "assets:bank:checking",
    "assets:bank:saving",
    "assets:cash",
    "expenses",
    "expenses:food",
    "expenses:supplies",
    "income",
    "income:gifts",
    "income:salary",
    "liabilities",
    "liabilities:debts"
]
```

Or all transactions:

```cli
$ curl -s http://127.0.0.1:5000/transactions | python3 -m json.tool
[
    {
        "tcode": "",
        "tcomment": "",
        "tdate": "2008-01-01",
        "tdate2": null,
        "tdescription": "income",
        "tindex": 1,
        "tpostings": [
            {
                "paccount": "assets:bank:checking",
                "pamount": [
                    {
                        "acommodity": "$",
                        "aismultiplier": false,
                        "aprice": null,
...
```

Most of the JSON corresponds to hledger's data types; for details of what the fields mean, see the
[Hledger.Data.Json haddock docs](https://hackage.haskell.org/package/hledger-lib/docs/Hledger-Data-Json.html)
and click on the various data types, eg
[Transaction](https://hackage.haskell.org/package/hledger-lib/docs/Hledger-Data-Types.html#t:Transaction).
And for a higher level understanding, see the [journal docs](hledger.md#journal).
There is also a basic [OpenAPI specification][openapi.yaml], also served at `/openapi.json`.

[openapi.yaml]: https://github.com/hledgerorg/hledger/blob/main/hledger-web/config/openapi.yaml

Some routes return a report type wrapping the data; eg `/accounttransactions` returns an
[AccountTransactionsReport](https://hackage.haskell.org/package/hledger-lib/docs/Hledger-Reports-AccountTransactionsReport.html#t:AccountTransactionsReport),
a report title and a list of items.
The handlers in [Hledger.Web.Handler.MiscR](https://hackage.haskell.org/package/hledger-web/docs/Hledger-Web-Handler-MiscR.html) show what each route returns.

You can add a new transaction to the journal with a PUT request to `/add`,
if hledger-web was started with `--allow=add` (the default when listening on a local-only address).
The payload must be the full, exact JSON representation of a hledger transaction
(partial data won't do).
You can get sample JSON from hledger-web's `/transactions` or `/accounttransactions`,
or from hledger's print command. Eg, this saves the first transaction of the sample journal to `txn.json`:

```cli
$ hledger -f examples/sample.journal print -O json | python3 -c 'import json,sys; json.dump(json.load(sys.stdin)[0], sys.stdout, indent=4)' > txn.json
```

Here's how it looks
(remember, this JSON corresponds to hledger's
[Transaction](https://hackage.haskell.org/package/hledger-lib/docs/Hledger-Data-Types.html#t:Transaction)
and related data types):

```json
{
    "tcode": "",
    "tcomment": "",
    "tdate": "2008-01-01",
    "tdate2": null,
    "tdescription": "income",
    "tindex": 1,
    "tpostings": [
        {
            "paccount": "assets:bank:checking",
            "pamount": [
                {
                    "acommodity": "$",
                    "acost": null,
                    "acostbasis": null,
                    "aquantity": {
                        "decimalMantissa": 1,
                        "decimalPlaces": 0,
                        "floatingPoint": 1
                    },
                    "astyle": {
                        "ascommodityside": "L",
                        "ascommodityspaced": false,
                        "asdecimalmark": ".",
                        "asdigitgroups": null,
                        "asprecision": 0,
                        "asrounding": "NoRounding"
                    }
                }
            ],
            "pbalanceassertion": null,
            "pcomment": "",
            "pdate": null,
            "pdate2": null,
            "poriginal": null,
            "preal": "RealPosting",
            "pstatus": "Unmarked",
            "ptags": [],
            "ptransaction_": "1"
        },
        {
            "paccount": "income:salary",
            "pamount": [
                {
                    "acommodity": "$",
                    "acost": null,
                    "acostbasis": null,
                    "aquantity": {
                        "decimalMantissa": -1,
                        "decimalPlaces": 0,
                        "floatingPoint": -1
                    },
                    "astyle": {
                        "ascommodityside": "L",
                        "ascommodityspaced": false,
                        "asdecimalmark": ".",
                        "asdigitgroups": null,
                        "asprecision": 0,
                        "asrounding": "NoRounding"
                    }
                }
            ],
            "pbalanceassertion": null,
            "pcomment": "",
            "pdate": null,
            "pdate2": null,
            "poriginal": null,
            "preal": "RealPosting",
            "pstatus": "Unmarked",
            "ptags": [],
            "ptransaction_": "1"
        }
    ],
    "tprecedingcomment": "",
    "tsourcepos": [
        {
            "sourceColumn": 1,
            "sourceLine": 31,
            "sourceName": "/home/user/hledger/examples/sample.journal"
        },
        {
            "sourceColumn": 1,
            "sourceLine": 34,
            "sourceName": "/home/user/hledger/examples/sample.journal"
        }
    ],
    "tstatus": "Unmarked",
    "ttags": []
}
```

And here's how to add it with curl. This should append a new entry to your journal file:

```cli
$ curl http://127.0.0.1:5000/add -X PUT -H 'Content-Type: application/json' --data-binary @txn.json
```

# DEBUG OUTPUT

You can add `--debug[=N]` to the command line to log debug output.
N ranges from 1 (least output, the default) to 9 (maximum output).
Typically you would start with 1 and increase until you are seeing enough.
Debug output goes to stderr, interleaved with the requests logged on stdout.
To capture debug output in a log file instead, you can usually redirect stderr, eg:\
`hledger-web --debug=3 2>hledger-web.log`.

# LANGUAGE

hledger-web's pages can be shown in another language when a translation catalog is available
(see [Languages](hledger.md#languages) in the hledger manual).

When hledger-web is started with `--lang LANG`, eg `--lang de`, every page is in that language,
whatever the browser asks for.
So you can use hledger-web in a language other than your browser's,
or give every user of a shared server the same language.

Otherwise the language is chosen per request, and the first of these naming an available translation wins:

1. a `_LANG` query parameter, eg `?_LANG=de`. This choice is remembered in a `_LANG` cookie.
2. the `_LANG` cookie
3. the browser's `Accept-Language` header, ie the browser's or system's language settings

If none does, pages are in English, or with `--lang auto`, in the language of hledger-web's environment.
So viewers usually get their browser's language automatically, if hledger-web has it.
Catalogs, including any in the config directory, are loaded when hledger-web starts.

# ENVIRONMENT

**LEDGER_FILE**
The main journal file to use when not specified with `-f/--file`.
Default: `$HOME/.hledger.journal`.

# BUGS

_reportbugs_

Some known issues:

Does not work well on small screens, or in text-mode browsers.
