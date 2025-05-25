:- module(tax_constants, [constants/3]).
:- use_module(library(csv)).

dep_contribution_reduction(DRC) :- 
    DRC = 642.73 * 12. % https://taxsummaries.pwc.com/luxembourg/individual/other-taxes

fd_reduction(198).

read_intervals(Filename, SortedIntervals) :-
    csv_read_file(Filename, Rows, []),
    maplist(row_to_interval, Rows, Intervals),
    sort(1, @>=, Intervals, SortedIntervals).

row_to_interval(Row, [IntX, FloatY]) :-
    Row =.. [_|[X, Y]],
    IntX is round(X),
    FloatY is float(Y).

intervals(Intervals) :-
    read_intervals('./app/data/intervals.csv', Intervals).

constants(Intervals, DepContributionReduction, FdReduction) :-
    intervals(Intervals),
    dep_contribution_reduction(DepContributionReduction),
    fd_reduction(FdReduction). 