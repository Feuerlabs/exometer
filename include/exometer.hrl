
-define(EXOMETER_TABLE, exometer:table(erlang:system_info(scheduler_id))).
-define(EXOMETER_SHARED, exometer_shared).

-record(exometer_event,
	{time = exometer:timestamp(),
	 from,
	 event}).

-record(exometer_entry, {
	  name,
	  type,
	  module = exometer,
	  status = enabled,
	  cache = 0,
	  value,
	  timestamp,
	  options = [],
	  ref}).

%% Used to redirect lookup from the scheduler-specific tables to the shared
-record(exometer_shared, { name }).
