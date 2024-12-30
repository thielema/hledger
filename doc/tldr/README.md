[tldr-pages](https://tldr.sh) provides very short, example-focussed doc pages for many command line tools, 
which can be viewed quickly with a `tldr` client
(such as [tealdeer](https://github.com/tealdeer-rs/tealdeer)).

tldr pages are short but high value, and a great counterbalance to PTA's verbose docs.
So they are worth prioritising and maintaining.
gutjuri made the first hledger tldr page in 2022, and sm added more in 2024.
We currently have 11 hledger-related tldr pages, covering
`hledger`, `hledger-ui`, `hledger-web`, and some of the most useful hledger commands.

## Contributing

The files in this directory (`doc/tldr/`) are copied periodically from 
the tldr-pages repo (`.../tldr-pages/pages/common/hledger-*`).
The tldr-pages repo has the [master copies](https://github.com/search?q=repo%3Atldr-pages%2Ftldr%20hledger&type=code),
and the pages are kept compliant with their style rules.


At the above github search, you can configure preferred languages, in the gear icon.
If you speak another language besides english, please consider [contributing a few translations](https://github.com/tldr-pages/tldr/blob/main/CONTRIBUTING.md#translations) !
It is relatively easy. You can do it entirely in your web browser if you have a Github account:

1. Starting with eg https://tldr.inbrowser.app/pages/common/hledger
2. click "Find this page on GitHub"
3. click the "Copy raw file" icon (two squares)
4. navigate to eg https://github.com/tldr-pages/tldr/tree/main/pages.de/common
5. click "Add file" > "Create new file"
6. reuse the original file name, paste the original content
7. translate the content
8. commit
9. send a pull request

Here is the [tldr translations status](https://lukwebsforge.github.io/tldri18n).
At the time of writing, translations have been contributed by

- kant (es)
- CodePsy-2001 (ko)

## Usage tips

Some of these pages are embedded in `hledger`, `hledger-ui` and `hledger-web`, accessible with `--tldr`.
We may find other ways to use them in future.

<https://tldr.inbrowser.app> is an online (& offline) tldr viewer,
where you can search for [hledger command examples](https://tldr.inbrowser.app/search?query=hledger+).
You can also make a web browser keyword for it ([firefox & chrome][1]; [safari][2]),
like `tldr CMD` = `https://tldr.inbrowser.app/search?query=CMD`.
Then in your browser's address bar you can type
`tldr`, `tldr hledger`, `tldr hledger-web`, `tldr hledger-balancesheet`, etc.

[1]: https://karl-voit.at/browser-keywords
[2]: http://safarikeywordsearch.aurlien.net
