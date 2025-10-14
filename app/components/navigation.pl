:- module(navigation, [nav_link//3, nav_link//2, top_nav_bar//0]).
:- use_module(library(http/html_write)).
:- use_module(render, [render_goals//1, render_results//1]).


nav_link(Link, Label, Tooltip) -->
    html(li([a([href(Link), class('text-gray-900 hover:underline'), title(Tooltip)], Label)])).

nav_link(Link, Label) -->
    html(li([a([href(Link), class('text-gray-900 hover:underline')], Label)])).

top_nav_bar(Content) -->
    html(header(class('sticky'), [
        nav(class('bg-white border-gray-200'), [
            div(class('flex flex-wrap justify-between items-center mx-auto max-w-screen-xl px-4 md:px-6 py-2.5'), [
                a([href('/'), class('flex items-center')], [
                    % img([src('https://flowbite.com/docs/images/logo.svg'), class('mr-3 h-6 sm:h-9'), alt('LetzCalc Logo')]),
                    span(class('self-center text-3xl whitespace-nowrap mr-2'), '🇱🇺'),
                    span(class('self-center text-xl font-semibold whitespace-nowrap'), 'LetzCalc')
                ]),
                div(class('flex items-center gap-4'), [
                    a([href('#'), class('text-sm font-medium sm:mr-6 text-primary-600 hover:underline')], 'Contact us'),
                    a([href('#'), class('text-sm font-medium text-primary-600 hover:underline sm:inline')], 'Login')
                ])
            ])
        ]),
        nav(class('bg-gray-50 dark:bg-gray-700'), [
            div(class('py-3 px-4 mx-auto max-w-screen-xl md:px-6'), [
                div(class('flex items-center'), [
                    ul(class('flex flex-row mt-0 mr-6 space-x-8 text-sm font-medium'), \render_goals(Content))
                ])
            ])
        ])
    ])).

top_nav_bar -->
    top_nav_bar([
        navigation:nav_link('/tax', 'Tax Calculator', 'Calculate your taxes with what-if analysis'),
        navigation:nav_link('/mortgage', 'Mortgage Calculator', 'Simulate mortgage payments and analyze different scenarios')
    ]).
