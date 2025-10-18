:- module(header, [header//1]).
:- use_module(library(http/html_write)).

header(Title) -->
    {string_concat(Title, '- LetzCalc 🇱🇺', FullTitle)},
    html([
        meta([charset('UTF-8')]),
        meta([name('viewport'), content('width=device-width, initial-scale=1.0')]),
        title(FullTitle),
        link([rel('stylesheet'), href('https://cdn.jsdelivr.net/npm/flowbite@3.1.2/dist/flowbite.min.css')]),
        link([rel('stylesheet'), href('/css/main.css')]),
        script([src('/js/twinspark.js'), type('text/javascript')], []),
        % script([src('/js/twinspark.min.js'), type('text/javascript')], []),
        script([src('https://cdn.jsdelivr.net/npm/flowbite@3.1.2/dist/flowbite.min.js')], []),
        script([type('text/javascript'), id(sfuw), async(true), src('https://cdn.jsdelivr.net/gh/StandForUkraine/site-widget@1.0/artifacts/index.iife.min.js?variant=strip&strip-color=black')], [])
    ]).

