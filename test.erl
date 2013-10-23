%% erl -config app.config


application:start(exometer).
exometer_admin:preset_defaults().
exometer_entry:new("test1", counter).
exometer_entry:new("test2", counter).

exometer_report:start_link().

exometer_report:subscribe(exometer_report_graphite, "test1", counter, 1000).

exometer_entry:update("test1", 1).
exometer_entry:update("test2", 2).



