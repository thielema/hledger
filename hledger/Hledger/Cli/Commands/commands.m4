m4_dnl _commands_
m4_dnl Used in the hledger manual to include all command docs.
m4_dnl Keep synced with Hledger.Cli.Commands.commandsList, hledger.m4.md.
m4_dnl The spaced colons avoid anchor/link collisions with similarly-named sections.
m4_define({{_commands_}}, 
{{
# Help commands

_command_({{Help}})
_command_({{Demo}})

# User interface commands

## ui

Runs [hledger-ui](hledger-ui.md) (if installed).

## web

Runs [hledger-web](hledger-web.md) (if installed).


# Data entry commands

_command_({{Add}})
_command_({{Import}})

# Basic report commands

_command_({{Accounts}})
_command_({{Codes}})
_command_({{Commodities}})
_command_({{Descriptions}})
_command_({{Files}})
_command_({{Notes}})
_command_({{Payees}})
_command_({{Prices}})
_command_({{Stats}})
_command_({{Tags}})

# Standard report commands

_command_({{Print}})
_command_({{Aregister}})
_command_({{Register}})
_command_({{Balancesheet}})
_command_({{Balancesheetequity}})
_command_({{Cashflow}})
_command_({{Incomestatement}})

# Advanced report commands

_command_({{Balance}})
_command_({{Roi}})

# Chart commands

_command_({{Activity}})

# Data generation commands

_command_({{Close}})
_command_({{Rewrite}})

# Maintenance commands

_command_({{Check}})
_command_({{Diff}})
_command_({{Test}})

}})m4_dnl
m4_dnl
m4_dnl _command_(MDFILE)
m4_dnl Include a command's doc. The doc should start with a level two heading. '
m4_define({{_command_}},
{{
m4_include(hledger/Hledger/Cli/Commands/$1.md)
}})m4_dnl
