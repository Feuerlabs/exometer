
-record(exometer_event,
	{time = exometer:timestamp(),
	 from,
	 event}).

-record(exometer_entry, {
	  name,
	  type,
	  module = exometer,
	  options = [],
	  ref}).

