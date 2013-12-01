%% erl -config app.config

lager:start().
application:start(exometer).

exometer:new([a,b,c], histogram).
exometer:new([a,b,d], counter).

exometer_report:subscribe(exometer_report_collectd, [a,b,c], max, 5000).
exometer_report:subscribe(exometer_report_collectd, [a,b,d], value, 3000).

lager:set_loglevel(lager_console_backend, debug).


oexometer_report:unsubscribe(exometer_report_collectd, [a,b,c], value).

exometer_report:list_metrics(['_',b,c]).

%% exometer_report:subscribe(exometer_report_graphite, [a,b,c], counter, c000).
%% exometer_report:subscribe(self(), [a,b,c], ms_since_reset, 2000).


exometer_report:list_metrics().

exometer:update([a,b,c], 1).
exometer:update([a,b,d], 2).


subscribe test_host a/b/c/min 5000 /tmp/test.ux
subscribe test_host a/b/c/max 5000 /tmp/test.ux

unsubscribe test_host a/b/c/min /tmp/test.ux
unsubscribe test_host a/b/c/max /tmp/test.ux
