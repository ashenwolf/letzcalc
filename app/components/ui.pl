:- module(ui, [container//1, card//1, app_button//2, input_field//3, dropdown_field//3, dropdown_option//2, page_title//1, tax_calculator_form//2, tax_calculator_container//3, gross_input//1, tax_class_option//3, render_results//1]).
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
tax_calculator_form(GrossValue, TaxClassValue) -->
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
tax_calculator_container(GrossValue, TaxClassValue, ResultsContentRuleOrAtom) -->
    html(div([id('tax-calculator-container'), class('grid grid-cols-1 md:grid-cols-2 gap-6')], [
        % This rule now generates a sequence of two sibling divs: tax-form and tax-results INSIDE the wrapper.
        div([id('tax-form')], [
            \tax_calculator_form(GrossValue, TaxClassValue)
        ]),
        div([id('tax-results'), class('bg-gray-50 p-4 rounded-lg')], [
            h3([class('text-lg font-semibold mb-2')], 'Results'),
            \render_results(ResultsContentRuleOrAtom)
        ])
    ])).

% Helper to render results: if it's a compound term, call it as a DCG rule, else treat as atom.
render_results(Rule) --> 
    { compound(Rule) }, % If it looks like a DCG rule call (e.g., results_content(...))
    !, % Cut to prevent backtracking to the atomic case if Rule is compound
    call(Rule). % Call the DCG rule
render_results(Atom) --> % Otherwise, it's an atom (e.g., the placeholder string)
    { atomic(Atom) },
    html(div([class('text-gray-500')], Atom)).
