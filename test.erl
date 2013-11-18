%% erl -config app.config

lager:start().
application:start(exometer).
exometer:new([a,b,1], histogram).
exometer:new([a,b,2], counter).
exometer_report:subscribe(exometer_report_collectd, [a,b,1], max, 5000).
exometer_report:subscribe(exometer_report_collectd, [a,b,2], value, 3000).

lager:set_loglevel(lager_console_backend, debug).


exometer_report:unsubscribe(exometer_report_collectd, [a,b,1], value).

exometer_report:list_metrics(['_',b,1]).

%% exometer_report:subscribe(exometer_report_graphite, [a,b,1], counter, 1000).
%% exometer_report:subscribe(self(), [a,b,1], ms_since_reset, 2000).


exometer_report:list_metrics().

exometer:update([a,b,1], 1).
exometer:update([a,b,2], 2).
