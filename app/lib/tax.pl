:- module(tax_lib, [calculate_tax/4]).

% Luxembourg tax rate brackets for 2023 (simplified)
% Format: gross_min, gross_max, rate
tax_bracket(0, 11265, 0).
tax_bracket(11266, 13173, 8).
tax_bracket(13174, 15009, 9).
tax_bracket(15010, 16881, 10).
tax_bracket(16882, 18753, 11).
tax_bracket(18754, 20625, 12).
tax_bracket(20626, 22569, 14).
tax_bracket(22570, 24513, 16).
tax_bracket(24514, 26457, 18).
tax_bracket(26458, 28401, 20).
tax_bracket(28402, 30345, 22).
tax_bracket(30346, 32289, 24).
tax_bracket(32290, 34233, 26).
tax_bracket(34234, 36177, 28).
tax_bracket(36178, 38121, 30).
tax_bracket(38122, 40065, 32).
tax_bracket(40066, 42009, 34).
tax_bracket(42010, 43953, 36).
tax_bracket(43954, 45897, 38).
tax_bracket(45898, 100000, 39).
tax_bracket(100001, 150000, 40).
tax_bracket(150001, 200000, 41).
tax_bracket(200001, 999999999, 42).

% Social security contributions
social_security_rate(0.125).  % 12.5% for health insurance, pension, etc.

calculate_tax(Gross, Tax, Net, Details) :-
    calculate_brackets(Gross, 0, Tax, BracketDetails),
    social_security_rate(SSRate),
    SocialSecurity is Gross * SSRate,
    Net is Gross - Tax - SocialSecurity,
    Details = json([
        gross=Gross,
        tax=Tax,
        social_security=SocialSecurity,
        net=Net,
        tax_brackets=BracketDetails
    ]).

calculate_brackets(Gross, AccTax, TotalTax, BracketDetails) :-
    findall(
        json([min=Min, max=Max, rate=Rate, amount=Amount]),
        (
            tax_bracket(Min, Max, Rate),
            Gross >= Min,
            (Gross >= Max -> 
                Amount is (Max - Min + 1) * (Rate / 100)
            ;
                Amount is (Gross - Min + 1) * (Rate / 100)
            ),
            Amount > 0
        ),
        BracketDetails
    ),
    sum_bracket_amounts(BracketDetails, AccTax, TotalTax).

sum_bracket_amounts([], Tax, Tax).
sum_bracket_amounts([json([min=_, max=_, rate=_, amount=Amount])|Rest], AccTax, TotalTax) :-
    NewAccTax is AccTax + Amount,
    sum_bracket_amounts(Rest, NewAccTax, TotalTax).

