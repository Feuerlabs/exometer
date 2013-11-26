%% -------------------------------------------------------------------
%%
%% Copyright (c) 2013 Basho Technologies, Inc.  All Rights Reserved.
%%
%%   This Source Code Form is subject to the terms of the Mozilla Public
%%   License, v. 2.0. If a copy of the MPL was not distributed with this
%%   file, You can obtain one at http://mozilla.org/MPL/2.0/.
%%
%% -------------------------------------------------------------------


-define(EXOMETER_TABLE, exometer_util:table(erlang:system_info(scheduler_id))).
-define(EXOMETER_SHARED, exometer_shared).
-define(EXOMETER_ENTRIES, exometer_entries).

-record(exometer_event,
	{time = exometer_util:timestamp(),
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
