:- module(home, [home_page//0]).

:- use_module('../components/ui').

home_page -->
    ui:container([
        \page_title('Lux Tax Calculator'),
        \grid(2, [
            \nav_card('/tax', 'Tax Calculator', 'Calculate your taxes with what-if analysis'),
            \nav_card('/mortgage', 'Mortgage Calculator', 'Simulate mortgage payments and analyze different scenarios')
        ])
    ]).
