# molar-mass.el

Molar-mass is an utility that calculates molar mass (aka molecular
mass or molecular weight) of a given molecule.

## Usage

It works interactively (entering your formula at the minibuffer) and
also with region.

`M-x molar-mass` goes to the minibuffer. Enter formula (Ex. H2O), it
returns:

"Molar mass of H2O : 18.015 g/mol (uma)"

It also accepts parenthesis at the formula. By example:

Fe(OH)2

"Molar mass of Fe(OH)2 : 89.860 g/mol (uma)"