:- module(home, [home_page//0]).

:- use_module(library(http/html_write)).
:- use_module(components/ui, []).
:- use_module(components/navigation, []).

home_page -->
    ui:html_page([
        navigation:top_nav_bar,
        ui:container([
            ui:card(
                ui:hero('Home', ui:paragraph('Welcome to the Home Page!'))
            )
        ])
    ]).
