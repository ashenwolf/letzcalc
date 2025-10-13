:- module(mortgage, [mortgage_page//0]).

:- use_module('../components/ui').

mortgage_page -->
    ui:container([
        \page_title('Mortgage Calculator'),
        \card([
            \form('/partial/mortgage/calculate', 'post', [
                \input_field('Loan Amount', 'amount', 'number'),
                \input_field('Interest Rate (%)', 'rate', 'number'),
                \input_field('Term (years)', 'term', 'number'),
                \button('Calculate', 'submit')
            ])
        ])
    ]). 