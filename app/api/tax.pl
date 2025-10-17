:- module(tax_api, [tax_calculate/1, tax_results_only/1, deductions_table_html/3, results_content//7]).

:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_json)).
:- use_module(library(http/html_write)).
:- use_module('../lib/tax').
:- use_module('../components/ui').
:- use_module('../components/render', [render_results//1]).

% Format currency with 2 decimal places
format_currency(Value, Formatted) :-
    format(string(NumStr), '~2f', [Value]),
    string_concat('€', NumStr, Formatted).

tax_calculate(Request) :-
    http_parameters(Request, [
        gross(Gross, [number, optional(true)]),
        net(Net, [number, optional(true)]),
        tax_class(TaxClass, [default('1')]),
        period(Period, [default('yearly')])
    ]),
    calculate_tax_for_period(Gross, Net, Period, Details),
    phrase(tax_response_html(TaxClass, Period, Details), HTMLTokens),
    format('Content-type: text/html~n~n'),
    print_html(HTMLTokens).

% Handler for tab switching - returns only the results section
tax_results_only(Request) :-
    http_parameters(Request, [
        gross(Gross, [number, optional(true)]),
        tax_class(TaxClass, [default('1')]),
        period(Period, [default('yearly')])
    ]),
    calculate_tax_for_period(Gross, Net, Period, Details),
    phrase(tax_results_response_html(Gross, TaxClass, Period, Details), HTMLTokens),
    format('Content-type: text/html~n~n'),
    print_html(HTMLTokens).

% Generate only the tax results section (for tab switching)
tax_results_response_html(Gross, TaxClass, Period, json([gross=GrossResult, tax=Tax, social_contribution=SocialContribution, net=Net, taxed_income=TaxedIncome, deductions=Deductions])) -->
    {
        format_currency(GrossResult, GrossF),
        format_currency(Tax, TaxF),
        format_currency(SocialContribution, SocialContributionF),
        format_currency(Net, NetF),
        format_currency(TaxedIncome, TaxedIncomeF),
        deductions_table_html(Deductions, DeductionsHTML, []),
        ResultsTerm = tax_api:results_content(GrossF, TaxF, SocialContributionF, NetF, TaxedIncomeF, DeductionsHTML, Period)
    },
    html(div([id('tax-results'), class('bg-gray-50 p-4 rounded-lg')], [
        \period_tabs_inline(Gross, TaxClass, Period),
        \render_results(ResultsTerm)
    ])).

% Inline DCG rules to avoid module call issues
period_tabs_inline(GrossValue, TaxClassValue, CurrentPeriod) -->
    html(div([class('mb-4')], [
        div([class('flex border-b border-gray-200')], [
            \period_tab_inline('yearly', 'Yearly', GrossValue, TaxClassValue, CurrentPeriod),
            \period_tab_inline('monthly', 'Monthly', GrossValue, TaxClassValue, CurrentPeriod)
        ])
    ])).

period_tab_inline(Period, Label, GrossValue, TaxClassValue, CurrentPeriod) -->
    {
        (Period = CurrentPeriod -> 
            Classes = 'px-4 py-2 text-sm font-medium text-blue-600 border-b-2 border-blue-600 bg-blue-50'
        ; 
            Classes = 'px-4 py-2 text-sm font-medium text-gray-500 hover:text-gray-700 hover:border-gray-300 border-b-2 border-transparent cursor-pointer'
        ),
        % Only make it clickable if we have a gross value
        (GrossValue \= '' ->
            format(atom(ActionURL), '/partial/tax/results?gross=~w&tax_class=~w&period=~w', [GrossValue, TaxClassValue, Period]),
            TwinSparkAttrs = [
                href=ActionURL,
                'ts-req'=ActionURL,
                'ts-swap'='morph',
                'ts-target'='#tax-results'
            ]
        ;
            TwinSparkAttrs = [href='#']
        )
    },
    html(a([
        class(Classes)
        |TwinSparkAttrs
    ], Label)).

% Calculate tax for specified period (yearly or monthly)
calculate_tax_for_period(Gross, Net, yearly, Details) :-
    AE = 0,
    lux_tax(yearly(Gross), yearly(Net, SocialContribution, AllDeductions, TaxedIncome, Tax, AE)),
    AllDeductions.healthcare = Healthcare,
    AllDeductions.healthcare_contrib = HealthcareContrib,
    AllDeductions.pension = Pension,
    AllDeductions.unemployment = Unemployment,
    Details = json([
        gross=Gross,
        tax=Tax,
        social_contribution=SocialContribution,
        net=Net,
        taxed_income=TaxedIncome,
        deductions=json([
            healthcare=Healthcare,
            healthcare_contrib=HealthcareContrib,
            pension=Pension,
            unemployment=Unemployment
        ])
    ]).

calculate_tax_for_period(Gross, Net, monthly, Details) :-
    AE = 0,
    lux_tax(yearly(Gross), monthly(Net, SocialContribution, AllDeductions, TaxedIncome, Tax, AE)),
    AllDeductions.healthcare = Healthcare,
    AllDeductions.healthcare_contrib = HealthcareContrib,
    AllDeductions.pension = Pension,
    AllDeductions.unemployment = Unemployment,
    MonthlyGross is Gross / 12,
    Details = json([
        gross=MonthlyGross,
        tax=Tax,
        social_contribution=SocialContribution,
        net=Net,
        taxed_income=TaxedIncome,
        deductions=json([
            healthcare=Healthcare,
            healthcare_contrib=HealthcareContrib,
            pension=Pension,
            unemployment=Unemployment
        ])
    ]).

% tax_response_html//4: DCG rule generating the #tax-calculator-container div
tax_response_html(TaxClass, Period, json([gross=GrossResult, tax=Tax, social_contribution=SocialContribution, net=Net, taxed_income=TaxedIncome, deductions=Deductions])) -->
    {
        format_currency(GrossResult, GrossF),
        format_currency(Tax, TaxF),
        format_currency(SocialContribution, SocialContributionF),
        format_currency(Net, NetF),
        format_currency(TaxedIncome, TaxedIncomeF),
        deductions_table_html(Deductions, DeductionsHTML, []),
        ResultsTerm = tax_api:results_content(GrossF, TaxF, SocialContributionF, NetF, TaxedIncomeF, DeductionsHTML, Period),
        GrossInt is round(GrossResult),
        NetInt is round(Net)
    },
    % Directly call the ui:tax_calculator_container DCG rule.
    % Its expansion (the div with id='tax-calculator-container') becomes the content generated by this DCG rule.
    ui:tax_calculator_container(GrossInt, NetInt, TaxClass, Period, ResultsTerm).

% DCG rule for results content. It will be called as tax_api:results_content by render_results//1 in ui.pl
results_content(GrossF, TaxF, SocialContributionF, NetF, TaxedIncomeF, DeductionsHTML, Period) -->
    {
        (Period = yearly -> PeriodLabel = 'Yearly' ; PeriodLabel = 'Monthly')
    },
    html([
        div([class('mb-4')], [
            h4([class('text-lg font-semibold text-blue-700')], [PeriodLabel, ' Calculation'])
        ]),
        table([class('w-full mb-4')], [
            tbody([],
                [
                    tr([], [
                        td([class('font-medium')], 'Gross Salary:'),
                        td([class('text-right')], [GrossF])
                    ]),
                    tr([], [
                        td([class('font-medium')], 'Taxed Income:'),
                        td([class('text-right')], [TaxedIncomeF])
                    ]),
                    tr([], [
                        td([class('font-medium')], 'Income Tax:'),
                        td([class('text-right')], [TaxF])
                    ]),
                    tr([], [
                        td([class('font-medium')], 'Social Contributions:'),
                        td([class('text-right')], [SocialContributionF])
                    ]),
                    tr([class('border-t border-gray-200 font-semibold')], [
                        td([], 'Net Salary:'),
                        td([class('text-right')], [NetF])
                    ])
                ])
        ]),
        h4([class('text-md font-semibold mt-4 mb-2')], 'Deductions Breakdown'),
        div([class('text-sm')], [DeductionsHTML])
    ]).

% Generate deductions table HTML
deductions_table_html(json([healthcare=Healthcare, healthcare_contrib=HealthcareContrib, pension=Pension, unemployment=Unemployment]), HTML, []) :-
    format_currency(Healthcare, HealthcareF),
    format_currency(HealthcareContrib, HealthcareContribF),
    format_currency(Pension, PensionF),
    format_currency(Unemployment, UnemploymentF),
    HTML = table([class('w-full text-xs')], [
        thead([],
            [tr([class('bg-gray-100')], [
                th([class('text-left py-1')], ['Deduction Type']),
                th([class('text-right py-1')], ['Amount'])
            ])
        ]),
        tbody([], [
            tr([class('border-t border-gray-100')], [
                td([class('py-1')], 'Healthcare (2.8%)'),
                td([class('text-right py-1')], [HealthcareF])
            ]),
            tr([class('border-t border-gray-100')], [
                td([class('py-1')], 'Healthcare Contribution (0.25%)'),
                td([class('text-right py-1')], [HealthcareContribF])
            ]),
            tr([class('border-t border-gray-100')], [
                td([class('py-1')], 'Pension (8%)'),
                td([class('text-right py-1')], [PensionF])
            ]),
            tr([class('border-t border-gray-100')], [
                td([class('py-1')], 'Unemployment (1.4%)'),
                td([class('text-right py-1')], [UnemploymentF])
            ])
        ])
    ]).
