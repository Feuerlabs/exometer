%%
%% Log macros
%%
-ifndef(__LOG_HRL__).
-define(__LOG_HRL__, true).

-compile({parse_transform, lager_transform}).

%% Lager logging levels
%%   debug, info, notice, warning, error, critical, alert, emergency, none.

-define(debug(Fmt), lager:debug(Fmt)).
-define(debug(Fmt, Args), lager:debug(Fmt, Args)).
-define(debug(Attrs, Fmt, Args), lager:debug(Attrs, Fmt, Args)).

-define(info(Fmt), lager:info(Fmt)).
-define(info(Fmt, Args), lager:info(Fmt, Args)).
-define(info(Attrs, Fmt, Args), lager:info(Attrs, Fmt, Args)).

-define(notice(Fmt), lager:notice(Fmt)).
-define(notice(Fmt, Args), lager:notice(Fmt, Args)).
-define(notice(Attrs, Fmt, Args), lager:notice(Attrs, Fmt, Args)).

-define(warning(Fmt), lager:warning(Fmt)).
-define(warning(Fmt, Args), lager:warning(Fmt, Args)).
-define(warning(Attrs, Fmt, Args), lager:warning(Attrs, Fmt, Args)).

-define(error(Fmt), lager:error(Fmt)).
-define(error(Fmt, Args), lager:error(Fmt, Args)).
-define(error(Attrs, Fmt, Args), lager:error(Attrs, Fmt, Args)).

-define(critical(Fmt), lager:critical(Fmt)).
-define(critical(Fmt, Args), lager:critical(Fmt, Args)).
-define(critical(Attrs, Fmt, Args), lager:critical(Attrs, Fmt, Args)).

-define(alert(Fmt), lager:alert(Fmt)).
-define(alert(Fmt, Args), lager:alert(Fmt, Args)).
-define(alert(Attrs, Fmt, Args), lager:alert(Attrs, Fmt, Args)).

-define(emergency(Fmt), lager:emergency(Fmt)).
-define(emergency(Fmt, Args), lager:emergency(Fmt, Args)).
-define(emergency(Attrs, Fmt, Args), lager:emergency(Attrs, Fmt, Args)).

-endif.
