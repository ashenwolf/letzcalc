% =============================================================================
% Lux Tax Calculator - Main Server
% 
% Development Workflow:
% - Start server: swipl -s app/server.pl -g 'server:start' or 'make start'
% - Server runs on http://localhost:8080
% - After changes to Prolog files: run 'make.' in SWI-Prolog console to reload
% - Use 'make reload' for quick predicate reloading without server restart
% 
% Directory Structure:
% - app/lib/: Business logic (tax calculations, constants)
% - app/components/: UI components (HTML generation predicates)  
% - app/pages/: Page controllers (request handling, routing)
% - app/data/: Data files (intervals.csv for tax brackets)
% - app/api/: API endpoints for AJAX/partial updates
% =============================================================================
:- module(server,[
    server/1            % ?Port
    ]).


:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_files)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_unix_daemon)).

:- use_module(components/header).
:- use_module(components/navigation).
:- use_module(components/ui).
:- use_module(pages/home).
:- use_module(pages/tax).
:- use_module(pages/mortgage).
:- use_module(api/tax).
:- use_module(api/mortgage).

% Define file locations
:- multifile http:location/3.
:- dynamic   http:location/3.

http:location(js, root(js), []).
http:location(css, root(css), []).
http:location(img, root(img), []).

% Define handlers
:- http_handler(root(.), home_handler, []).
:- http_handler(root(tax), tax_handler, []).
:- http_handler(root(mortgage), mortgage_handler, []).
:- http_handler(root(partial/tax/calculate), tax_calculate, [method(post)]).
:- http_handler(root(partial/tax/results), tax_results_only, [methods([get, post])]).
:- http_handler(root(partial/mortgage/calculate), mortgage_calculate, [method(post)]).

% Static file handlers
:- http_handler(js(.), http_reply_from_files('public/js', []), [prefix]).
% :- http_handler(css(.), http_reply_from_files('public/css', []), [prefix]).
% :- http_handler(img(.), http_reply_from_files('public/img', []), [prefix]).

home_handler(_Request) :-
    reply_html_page(
        \header('Lux Tax Calculator'),
        \home_page
    ).

tax_handler(_Request) :-
    reply_html_page(
        \header('Tax Calculator'),
        \tax_page
    ).

mortgage_handler(_Request) :-
    reply_html_page(
        \header('Mortgage Calculator'),
        \mortgage_page
    ).

server(Port) :-
    http_server(http_dispatch,
        [ port(Port),
          workers(16)
        ]).
