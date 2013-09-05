
-define(EXOMETER_TABLE, exometer:table(erlang:system_info(scheduler_id))).

-record(exometer_event,
	{time = exometer:timestamp(),
	 from,
	 event}).

-record(exometer_entry, {
	  name,
	  type,
	  module = exometer,
	  options = [],
	  mod_state = undefined}).

