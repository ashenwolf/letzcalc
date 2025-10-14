:- module(render, [render_goals//1, render_results//1]).
:- use_module(library(http/html_write)).

render_goals([]) --> [].
render_goals([Goal|Rest]) -->
    ( { Goal = html(_) } ->
        call(Goal)              % It's already HTML
    ;
        html(\call(Goal))       % Wrap non-HTML DCGs
    ),
    render_goals(Rest).
render_goals(Goal) -->
    { \+ is_list(Goal) },       % Not a list, so it's a single goal
    render_goals([Goal]).


% Helper to render results: if it's a compound term, call it as a DCG rule, else treat as atom.
render_results(Rule) --> 
    { compound(Rule) }, % If it looks like a DCG rule call (e.g., results_content(...))
    !, % Cut to prevent backtracking to the atomic case if Rule is compound
    call(Rule). % Call the DCG rule
render_results(Atom) --> % Otherwise, it's an atom (e.g., the placeholder string)
    { atomic(Atom) },
    html(Atom).
