:- module(home, [home_page//0]).

:- use_module('../components/ui').

home_page -->
    ui:container([
        \page_title('Lux Tax Calculator'),
        div([class('grid grid-cols-1 md:grid-cols-2 gap-6')],
            [
                a([href('/tax'), class('block p-6 bg-white border border-gray-200 rounded-lg shadow hover:bg-gray-100 dark:bg-gray-800 dark:border-gray-700 dark:hover:bg-gray-700')],
                    [
                        h2([class('mb-2 text-2xl font-bold tracking-tight text-gray-900 dark:text-white')], 'Tax Calculator'),
                        p([class('font-normal text-gray-700 dark:text-gray-400')], 'Calculate your taxes with what-if analysis')
                    ]),
                a([href('/mortgage'), class('block p-6 bg-white border border-gray-200 rounded-lg shadow hover:bg-gray-100 dark:bg-gray-800 dark:border-gray-700 dark:hover:bg-gray-700')],
                    [
                        h2([class('mb-2 text-2xl font-bold tracking-tight text-gray-900 dark:text-white')], 'Mortgage Calculator'),
                        p([class('font-normal text-gray-700 dark:text-gray-400')], 'Simulate mortgage payments and analyze different scenarios')
                    ])
            ])
    ]).
