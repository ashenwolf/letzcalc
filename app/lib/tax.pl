% =============================================================================
% Tax Calculation Library - Business Logic
% 
% Main Predicates:
% - lux_tax/2: Advanced tax calculation with detailed breakdowns
% - calculate_tax/4: Legacy interface for backward compatibility
% 
% Usage Examples:
% - lux_tax(yearly(50000), yearly(Net, SocialContrib, AllDeductions, TaxedIncome, Tax, AE))
% - lux_tax(monthly(4167), monthly(Net, SocialContrib, AllDeductions, TaxedIncome, Tax, AE))
% 
% Development Notes:
% - Modify tax rates and brackets in ../data/intervals.csv
% - Update social security rates and constants in tax_constants.pl
% - Always test thoroughly - affects real money calculations
% =============================================================================

:- module(tax_lib, [calculate_tax/4, lux_tax/2]).
:- use_module(library(clpr)).
:- use_module(library(apply)).
:- use_module(tax_constants).

% Social security deductions
deduction_healthcare(Gross, Deduction):-
    {Deduction = Gross * 0.028}.

deduction_healthcare_contrib(Gross, Deduction):-
    {Deduction = Gross * 0.0025}.

deduction_pension(Gross, Deduction):-
    {Deduction = Gross * 0.08}.

deduction_unemployment(Gross, Deduction):-
    tax_constants:constants(_, DCR, _),
    {Deduction = (Gross - DCR) * 0.014}.

% Tax calculation using intervals
get_intervals(_, TotalTax, []):-
    {TotalTax=0}.

get_intervals(Sum, TotalTax, [CurrentInterval|NextIntervals]):-
    CurrentInterval=[CurrentLimit|_],
    {Sum < CurrentLimit},
    get_intervals(Sum, TotalTax, NextIntervals).
    
get_intervals(Sum, TotalTax, [CurrentInterval|NextIntervals]):-
    CurrentInterval=[CurrentLimit, CurrentTax|_],
    {
        Sum >= CurrentLimit,
        Taxable = Sum - CurrentLimit,
        Tax = Taxable * CurrentTax * 1.07
    },
    get_intervals(CurrentLimit, RemainingTax, NextIntervals),
    {TotalTax = Tax + RemainingTax}.

get_intervals(Sum, TotalTax):-
    tax_constants:constants(Int, _, _),
    get_intervals(Sum, TotalTax, Int).

to_month(ValueY, ValueM):-
    {ValueM = ValueY / 12}.
    
convert_pair(Key-YearlyValue, Key-MonthlyValue) :-
    to_month(YearlyValue, MonthlyValue).

% Main tax calculation predicate (new sophisticated version)
lux_tax(yearly(Gross), yearly(Net, SocialContribution, AllDeductions, TaxedIncome, Tax, AE)):-
    tax_constants:constants(_, _, FD),
    deduction_healthcare(Gross, Healthcare),
    deduction_healthcare_contrib(Gross, HealthcareContrib),
    deduction_pension(Gross, Pension),
    deduction_unemployment(Gross, Unemployment),
    AllDeductions=deductions{
        healthcare: Healthcare,
        healthcare_contrib: HealthcareContrib,
        pension: Pension,
        unemployment: Unemployment
    },

    {
        Deductions = Healthcare + HealthcareContrib + Pension,
        SocialContribution = Deductions + Unemployment,
        TaxedIncome = Gross - Deductions - FD - AE
    },
    
    get_intervals(TaxedIncome, Tax),
    
    {
        Net = (TaxedIncome - Tax - Unemployment)
    }.

lux_tax(yearly(Gross), monthly(Net, Deductions, AllDeductions, TaxedIncome, Tax, AE)):-
    lux_tax(yearly(Gross), yearly(NetY, DeductionsY, AllDeductionsY, TaxedIncomeY, TaxY, AEY)),
    dict_pairs(AllDeductionsY, deductions, DeductionFieldsY),
    maplist(convert_pair, DeductionFieldsY, DeductionFieldsM),
    dict_pairs(AllDeductions, deductions, DeductionFieldsM),
    to_month(NetY, Net),
    to_month(DeductionsY, Deductions),
    to_month(TaxedIncomeY, TaxedIncome),
    to_month(TaxY, Tax),
    to_month(AEY, AE).

lux_tax(monthly(Gross), monthly(Net, Deductions, AllDeductions, TaxedIncome, Tax, AE)):-
    {GrossY = Gross * 12},
    lux_tax(yearly(GrossY), monthly(Net, Deductions, AllDeductions, TaxedIncome, Tax, AE)).

lux_tax(monthly(Gross), yearly(Net, Deductions, AllDeductions, TaxedIncome, Tax, AE)):-
    {GrossY = Gross * 12},
    lux_tax(yearly(GrossY), yearly(Net, Deductions, AllDeductions, TaxedIncome, Tax, AE)).

% Legacy interface for backward compatibility
calculate_tax(Gross, Tax, Net, Details) :-
    AE = 0, % No additional exemptions for basic calculation
    lux_tax(yearly(Gross), yearly(NetCalc, SocialContribution, AllDeductions, TaxedIncome, TaxCalc, AE)),
    Tax = TaxCalc,
    Net = NetCalc,
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

