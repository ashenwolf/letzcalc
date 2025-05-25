:- module(mortgage_api, [mortgage_calculate/2]).

:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_json)).

mortgage_calculate(Request, Response) :-
    http_parameters(Request, [
        amount(Amount, [number]),
        rate(Rate, [number]),
        term(Term, [number])
    ]),
    % TODO: Implement actual mortgage calculation
    Response = json([
        amount=Amount,
        rate=Rate,
        term=Term,
        monthly_payment=0
    ]). 