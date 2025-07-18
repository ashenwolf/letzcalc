:- module(mortgage, [mortgage_page//0]).

:- use_module('../components/ui').

mortgage_page -->
    ui:container([
        ui:page_title('Mortgage Calculator'),
        ui:card([
            form(class('space-y-6'), action('/partial/mortgage/calculate'), method('post'),
                [
                    ui:input_field('Loan Amount', 'amount', 'number'),
                    ui:input_field('Interest Rate (%)', 'rate', 'number'),
                    ui:input_field('Term (years)', 'term', 'number'),
                    ui:button('Calculate', 'submit')
                ])
        ])
    ]). 