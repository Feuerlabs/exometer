%% erl -config app.config


application:start(exometer).
exometer_admin:preset_defaults().
exometer_entry:new("test", counter).
exometer_report:start_link().
exometer_report:subscribe(exometer_report_graphite, "test", counter, 1000).


exometer_entry:update("test", 1).
