:- module(tax, [tax_page//0]).
:- use_module(library(http/html_write)).

:- use_module(components/ui).

tax_page -->
    ui:html_page([
        navigation:top_nav_bar,
        ui:container([
            ui:page_title('Tax Calculator'),
            ui:card([
                ui:tax_calculator_container('', '1', 'yearly', 'Enter a gross salary and click Calculate to see tax breakdown')
            ])
        ])
    ]).
