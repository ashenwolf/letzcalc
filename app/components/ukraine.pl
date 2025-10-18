:- module(ukraine, [ukraine_banner//0]).
:- use_module(library(http/html_write)).

ukraine_banner -->
    html(
        a([href="XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX", target="_blank", rel="nofollow noopener", title="Donate to support Ukraine's independence."], [
            div([class="support-ukraine", role="banner"], [
                div([class="support-ukraine__flag", role="img", ariaLabel="Flag of Ukraine"], [
                    div([class="support-ukraine__flag__blue"], []),
                    div([class="support-ukraine__flag__yellow"], [])
                ]),
                div([class="support-ukraine__label"], [
                    text("Donate to support Ukraine's independence.")
                ])
            ])
        ])
    ).

% <a class="support-ukraine" href="https://help.unicef.org/ukraine-emergency" target="_blank" rel="nofollow noopener" title="Donate to support Ukraine's independence.">
% 	<div class="support-ukraine__flag" role="img" aria-label="Flag of Ukraine">
% 		<div class="support-ukraine__flag__blue"></div>
% 		<div class="support-ukraine__flag__yellow"></div>
% 	</div>
% 	<div class="support-ukraine__label">
% 		Donate to support Ukraine's independence.
% 	</div>
% </a>