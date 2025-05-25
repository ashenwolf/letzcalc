:- module(tax, [tax_page//0]).

:- use_module('../components/ui').
:- use_module(library(http/html_write)).

tax_page -->
    ui:container([
        \page_title('Tax Calculator'),
        \card([
            \tax_calculator_container('', '1', 'yearly', 'Enter a gross salary and click Calculate to see tax breakdown')
        ])
    ]).