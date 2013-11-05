%% erl -config app.config


application:start(exometer).
exometer_admin:preset_defaults().
exometer:new([a,b,1], counter).
exometer:new([a,b,2], counter).
exometer_report:start_link().

exometer_report:subscribe(exometer_report_collectd, [a,b,1], value, 5000).

exometer_report:subscribe(exometer_report_collectd, [a,b,2], value, 3000).
exometer_report:unsubscribe(exometer_report_collectd, [a,b,1], value).

exometer_report:list_metrics(['_',b,1]).

%% exometer_report:subscribe(exometer_report_graphite, [a,b,1], counter, 1000).
%% exometer_report:subscribe(self(), [a,b,1], ms_since_reset, 2000).


exometer_report:list_metrics().

exometer_entry:update([a,b,1], 1).
exometer_entry:update([a,b,2], 2).
