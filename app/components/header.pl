:- module(header, [header//1]).
:- use_module(library(http/html_write)).

header(Title) -->
    html([
        meta([charset('UTF-8')]),
        meta([name('viewport'), content('width=device-width, initial-scale=1.0')]),
        title(Title),
        link([rel('stylesheet'), href('https://cdn.jsdelivr.net/npm/flowbite@3.1.2/dist/flowbite.min.css')]),
        script([src('/js/twinspark.js'), type('text/javascript')], []),
        % script([src('/js/twinspark.min.js'), type('text/javascript')], []),
        script([src('https://cdn.jsdelivr.net/npm/flowbite@3.1.2/dist/flowbite.min.js')], [])
    ]).
