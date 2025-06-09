% =============================================================================
% Tax Constants and Configuration
% 
% This module provides tax-related constants for Luxembourg tax calculations:
% - Tax brackets and rates (loaded from ../data/intervals.csv)
% - Social security contribution limits
% - Standard deductions and reductions
% 
% To modify tax rates:
% 1. Edit ../data/intervals.csv (format: income_threshold,tax_rate)
% 2. Reload predicates with 'make.' in SWI-Prolog console
% 
% Constants:
% - DCR: Dependency Contribution Reduction (€642.73 * 12 annually)
% - FD: Standard tax reduction (€198)
% =============================================================================

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