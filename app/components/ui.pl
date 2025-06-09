% =============================================================================
% UI Components Library
% 
% Reusable HTML generation predicates using Tailwind CSS classes.
% All styling should use Tailwind CSS classes only - no custom CSS.
% 
% Main Components:
% - container//1: Main page container with responsive padding
% - card//1: White card component with border and shadow
% - tax_calculator_form//3: Complete tax calculator form
% - tax_results_with_tabs//4: Results display with period tabs
% 
% Development Notes:
% - Always use Tailwind CSS classes for styling
% - Test responsive design on different screen sizes
% - Use semantic HTML elements for accessibility
% =============================================================================

:- module(ui, [container//1, card//1, app_button//2, input_field//3, dropdown_field//3, dropdown_option//2, page_title//1, tax_calculator_form//3, tax_calculator_container//4, gross_input//1, tax_class_option//3, render_results//1, period_tabs//3, period_tab//5, tax_results_with_tabs//4]).
:- use_module(library(http/html_write)).

container(Content) -->
    html(div([class('max-w-4xl mx-auto p-4 sm:p-6 lg:p-8')], Content)).

card(Content) -->
    html(div([class('bg-white rounded-lg border border-gray-200 shadow-sm p-2')], Content)).

app_button(Text, Type) -->
    html(button([type(Type), class('text-white bg-blue-700 hover:bg-blue-800 focus:ring-4 focus:ring-blue-300 font-medium rounded-lg text-sm px-5 py-2.5 focus:outline-none')], Text)).

input_label(Text) -->
    html(label([class('block mb-2 text-sm font-medium text-gray-900')], Text)).

input_field(Label, Name, Type) -->
    html(div([
        \input_label(Label),
        input([type(Type), name(Name), class('bg-gray-50 border border-gray-300 text-gray-900 text-sm rounded-lg focus:ring-blue-500 focus:border-blue-500 block w-full p-2.5')])
    ])).

dropdown_field(Label, Name, Options) -->
    html(div([
        \input_label(Label),
        select([name(Name), class('bg-gray-50 border border-gray-300 text-gray-900 text-sm rounded-lg focus:ring-blue-500 focus:border-blue-500 block w-full p-2.5')], Options)
    ])).

dropdown_option(Text, Value) -->
    html(option([value(Value)], Text)).

page_title(Text) -->
    html(h1([class('mb-4 text-4xl font-extrabold leading-none tracking-tight text-gray-900 md:text-5xl lg:text-6xl')], Text)).

% Reusable tax calculator form component
tax_calculator_form(GrossValue, TaxClassValue, Period) -->
    html(form([
        class('space-y-6'), 
        id('tax-calculator'), 
        action('/partial/tax/calculate'),
        method('POST'),
        'ts-req',
        'ts-swap'='morph',
        'ts-target'='#tax-calculator-container'
    ], [
        div([
            label([class('block mb-2 text-sm font-medium text-gray-900')], 'Gross Salary (€)'),
            \gross_input(GrossValue)
        ]),
        div([
            label([class('block mb-2 text-sm font-medium text-gray-900')], 'Tax Class'),
            select([name('tax_class'), class('bg-gray-50 border border-gray-300 text-gray-900 text-sm rounded-lg focus:ring-blue-500 focus:border-blue-500 block w-full p-2.5')], [
                \tax_class_option('Class 1', '1', TaxClassValue),
                \tax_class_option('Class 1a', '1a', TaxClassValue),
                \tax_class_option('Class 2', '2', TaxClassValue)
            ])
        ]),
        input([type('hidden'), name('period'), value(Period), id('period-input')]),
        button([type('submit'), class('text-white bg-blue-700 hover:bg-blue-800 focus:ring-4 focus:ring-blue-300 font-medium rounded-lg text-sm px-5 py-2.5 focus:outline-none')], 'Calculate')
    ])).

% Helper for gross input with optional value
gross_input('') -->
    html(input([type('number'), name('gross'), class('bg-gray-50 border border-gray-300 text-gray-900 text-sm rounded-lg focus:ring-blue-500 focus:border-blue-500 block w-full p-2.5')])).
gross_input(Value) -->
    { Value \= '' },
    html(input([type('number'), name('gross'), value(Value), class('bg-gray-50 border border-gray-300 text-gray-900 text-sm rounded-lg focus:ring-blue-500 focus:border-blue-500 block w-full p-2.5')])).

% Helper for tax class option with selected state
tax_class_option(Text, Value, SelectedValue) -->
    { (Value = SelectedValue -> Selected = [selected] ; Selected = []) },
    html(option([value(Value)|Selected], Text)).

% Complete tax calculator container - now WITH the outer #tax-calculator-container div
tax_calculator_container(GrossValue, TaxClassValue, Period, ResultsContentRuleOrAtom) -->
    html(div([id('tax-calculator-container'), class('grid grid-cols-1 md:grid-cols-2 gap-6')], [
        % This rule now generates a sequence of two sibling divs: tax-form and tax-results INSIDE the wrapper.
        div([id('tax-form')], [
            \tax_calculator_form(GrossValue, TaxClassValue, Period)
        ]),
        div([id('tax-results'), class('bg-gray-50 p-4 rounded-lg')], [
            \tax_results_with_tabs(GrossValue, TaxClassValue, Period, ResultsContentRuleOrAtom)
        ])
    ])).

% Tax results with tabs - this is the second render group
tax_results_with_tabs(GrossValue, TaxClassValue, Period, ResultsContentRuleOrAtom) -->
    html([
        \period_tabs(GrossValue, TaxClassValue, Period),
        \render_results(ResultsContentRuleOrAtom)
    ]).

% Add period tabs for switching between yearly and monthly
period_tabs(GrossValue, TaxClassValue, CurrentPeriod) -->
    html(div([class('mb-4')], [
        div([class('flex border-b border-gray-200')], [
            \period_tab('yearly', 'Yearly', GrossValue, TaxClassValue, CurrentPeriod),
            \period_tab('monthly', 'Monthly', GrossValue, TaxClassValue, CurrentPeriod)
        ])
    ])).

period_tab(Period, Label, GrossValue, TaxClassValue, CurrentPeriod) -->
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

% Helper to render results: if it's a compound term, call it as a DCG rule, else treat as atom.
render_results(Rule) --> 
    { compound(Rule) }, % If it looks like a DCG rule call (e.g., results_content(...))
    !, % Cut to prevent backtracking to the atomic case if Rule is compound
    call(Rule). % Call the DCG rule
render_results(Atom) --> % Otherwise, it's an atom (e.g., the placeholder string)
    { atomic(Atom) },
    html(div([class('text-gray-500')], Atom)).
