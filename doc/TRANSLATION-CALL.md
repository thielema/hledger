# Translation call for hledger 1.99.6

To: hledger@googlegroups.com
Cc: Daniil Iaitskov <dyaitskov@gmail.com>,
    Chunhui Ouyang <jack9603301@qhjack.top>
Subject: Translators wanted for hledger 1.99.6 (deadline Sat Oct 31)

Hi all,

hledger 1.99.6, AKA 2.0 preview 6, is due on Nov 1 or soon after,
and ideally its translations will be as complete as possible.

hledger can currently show its reports, hledger-ui and hledger-web in German,
Spanish, Russian and Simplified Chinese. Each of these catalogs now has
220 untranslated entries, about 870 words, nearly all new hledger-web
text: the help dialog, the options panel, the chart controls and the key
menus. 173 of them carry a note explaining where the text appears. The
English text is now (soft-)frozen until the release, and the catalogs on main
are up to date with it:

  https://github.com/hledgerorg/hledger/tree/main/hledger-lib/locale

Help wanted, by Saturday Oct 31, 20:00 UTC:

- German (de): update. Vocabulary was chosen by Henning Thielemann.
- Spanish (es): a native speaker to review the file, which was
  machine-translated and has not yet been checked by a person, and to
  translate the new entries.
- Russian (ru): update. Translated by Daniil Iaitskov, with some
  improvements by Dmitry Astapov.
- Simplified Chinese (zh-Hans): update. Translated by Chunhui Ouyang.
- New languages are welcome at any time.

To avoid two people doing the same work, please reply to the list
saying which language you are taking on, before you start, and check
the replies for one already claimed. Past translators of a language
have precedence; if you would like to help them, coordinate with them
on the list (a second reader is always useful).

Partial updates are welcome. Before release, I'll fill in any gaps with
machine translations, each marked with a comment, so that they can be found
and reviewed later.

Translations arriving after the deadline will still be useful for the next release.

How to:

- No programming is needed; the guide is https://hledger.org/TRANSLATING.html.
  Start from your language's file in the current hledger main branch (not an older copy).
- Most of the new text is in hledger-web, which the installed 1.99.5
  does not have yet. To see it in context you'd need to build main;
  otherwise translating from the notes is fine, and I'll check the pages
  when merging.
- Send it back as a pull request, or a reply with the .po file attached.

Thanks!
-Simon
