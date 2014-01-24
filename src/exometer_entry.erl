%% -------------------------------------------------------------------
%%
%% Copyright (c) 2013 Basho Technologies, Inc.  All Rights Reserved.
%%
%%   This Source Code Form is subject to the terms of the Mozilla Public
%%   License, v. 2.0. If a copy of the MPL was not distributed with this
%%   file, You can obtain one at http://mozilla.org/MPL/2.0/.
%%
%% -------------------------------------------------------------------
%% @doc
%% == Creating custom exometer entries ==
%% 
%% An exometer_entry behavior implementation can be created when custom
%% processing of various metrics is needed.
%% 
%% A custom exometer entry is invoked by mapping a type to the module
%% name of the custom exometer entry module. All metrics created with the
%% given type will trigger the invocation of the new entry module. See
%% {@section Configuring type - entry maps} for details on how to setup
%% such maps.
%% 
%% The life cycle of a an exometer entry consists of the following steps.
%% 
%% + Metrics Creation
%%     <br/>`new/3' is invoked by exometer to signal that a new metrics
%%     should be created. The name of the new metric will be provided as
%%     a list of atoms.
%% 
%% + Update Data
%%     <br/>Values will be sent to the entry through the `update/4'
%%     function. The custom entry should store this value for the given
%%     metric and break it down into data points that can be reported for
%%     the metric.
%% 
%% + Retrieve Value
%%     <br/>`get_value/4' will be invoked by exometer to retrieve specific
%%     data points from a given metric. 
%% 
%% See individual functions for details on the
%% in the exometer_entry behavior.
%% 
%% === new/3 ===
%% 
%% The `new()' function is invoked as follows:
%% 
%% <pre lang="erlang">
%%      new(Name, Type, Options)</pre>
%% 
%% The custom entry should create the necessary state for the new metric and store
%% it for furure access through `update()' and `get_value()' calls. 
%% 
%% + `Name'
%%     <br/>Specifies the name of the metric to be created as a list of atoms. 
%% 
%% + `Type'
%%     <br/>Specifies the type provided to the `exometer:new()' call (before it
%%     was translated by the type - exometer entry map). It can be used if several
%%     different types are mapped to the same entry module.
%% 
%% + `Options'
%%     <br/>Specifies an option list that contains additional setup directives to
%%     the entry. The actual options to support are implementation dependent.
%% 
%% The `new()' function should return `{ok, Ref}' where Ref is a
%% tuple that will be provided as a reference argument to other calls
%% made into the module. Any other return formats will cancel the
%% creation of the new metric.
%% 
%% === delete/3 ===
%% 
%% The `delete()' function is invoked as follows:
%% 
%% <pre lang="erlang">
%%      delete(Name, Type, Ref)</pre>
%% 
%% The custom entry should free all resources associated with the given name.
%% 
%% + `Name'
%%     <br/>Specifies the name of the metric to be deleted as a list of atoms. 
%% 
%% + `Type'
%%     <br/>Specifies the type provided to the `exometer:new()' call (before it
%%     was translated by the type - exometer entry map). 
%% 
%% + `Ref'
%%     <br/>Will contain the same tuple returned as `Ref' by the module's `new()' function.
%% 
%% The `delete()' function shall return `ok'.
%% 
%% 
%% === get_value/4 ===
%% 
%% The `get_value()' function is invoked as follows:
%% 
%% <pre lang="erlang">
%%      get_value(Name, Type, Ref, DataPoints)</pre>
%% 
%% The custom entry should retrieve the metric with the given name and
%% return the values of the specified data points. Data points can be
%% expected to be one or more of those returned by the entry's
%% `get_datapoints()' function.
%% 
%% + `Name'
%%     <br/>Specifies the name of the metric to update with a value. 
%% 
%% + `Type'
%%     <br/>Specifies the type provided to the `exometer:new()' call (before it
%%     was translated by the type - exometer entry map). 
%% 
%% + `Ref'
%%     <br/>Will contain the same tuple returned as `Ref' by the module's `new()' function.
%% 
%% + `DataPoints'
%%     <br/>Will contain a list of data points, each picked from the list returned by
%%     the module's `get_datapoints()' function. 
%% 
%% The `get_value()' function should calculate the values of the given
%% data points based on previous calls to `update()' and return them to the caller.
%% 
%% The return format shall be:
%% 
%% <pre lang="erlang">
%%      {ok, [ { DataPoint, Value }, ...]}</pre>
%% 
%% Each `{ DataPoint, Value }' tuple shall contain the name and value of
%% one of the data points provided as arguments to `get_value()'.
%% 
%% If a data point is not valid (i.e. not in the list returned by
%% `get_datapoints()'), the returned tuple should be `{ DataPoint,
%% undefined }'.
%% 
%% 
%% === update/4 ===
%% 
%% The `update()' function is invoked as follows:
%% 
%% <pre lang="erlang">
%%      update(Name, Value, Type, Ref)</pre>
%% 
%% + `Name'
%%     <br/>Specifies the name of the metric to update. 
%% 
%% + `Value'
%%     <br/>Specifies the new value to integrate into the given metric. 
%% 
%% + `Type'
%%     <br/>Specifies the type provided to the `exometer:new()' call (before it
%%     was translated by the type - exometer entry map). 
%% 
%% + `Ref'
%%     <br/>Will contain the same tuple returned as `Ref' by the module's `new()' function.
%% 
%% The `update()' function should update the data points for the metric with the
%% given name in preparation for future calls to `get_value()'.
%% 
%% The return format shall be `ok'.
%% 
%% 
%% === reset/3 ===
%% 
%% The `reset()' function is invoked as follows:
%% 
%% <pre lang="erlang">
%%      reset(Name, Type, Ref)</pre>
%% 
%% + `Name'
%%     <br/>Specifies the name of the metric to reset. 
%% 
%% + `Type'
%%     <br/>Specifies the type provided to the `exometer:new()' call (before it
%%     was translated by the type - exometer entry map). 
%% 
%% + `Ref'
%%     <br/>Will contain the same tuple returned as `Ref' by the module's `new()' function.
%% 
%% The `reset()' function should revert the metric with the given name to
%% its original state. A counter, for example, should be reset to 0 while
%% histograms should be emptied.
%% 
%% The return format shall be `ok'.
%% 
%% === sample/3 ===
%% 
%% The `sample()' function is invoked as follows:
%% 
%% <pre lang="erlang">
%%      sample(Name, Type, Ref)</pre>
%% 
%% + `Name'
%%     <br/>Specifies the name of the metric to run the sample. 
%% 
%% + `Type'
%%     <br/>Specifies the type provided to the `exometer:new()' call (before it
%%     was translated by the type - exometer entry map). 
%% 
%% + `Ref'
%%     <br/>Will contain the same tuple returned as `Ref' by the module's
%%     `new()' function.
%% 
%% This function is only used by probes, where it is periodically called
%% to sample a local sub system such as /proc or netlink in order to
%% update its data points.
%% 
%% Any exometer entry-based implementation should do nothing and return
%% `ok'.
%% 
%% === get_datapoints/3 ===
%% 
%% The `get_datapoints()' function is invoked as follows:
%% 
%% <pre lang="erlang">
%%      get_datapoints(Name, Type, Ref)</pre>
%% 
%% + `Name'
%%     <br/>Specifies the name of the metric to return available datapoints for.
%% 
%% + `Type'
%%     <br/>Specifies the type provided to the `exometer:new()' call (before it
%%     was translated by the type - exometer entry map). 
%% 
%% + `Ref'
%%     <br/>Will contain the same tuple returned as `Ref' by the module's
%%     `new()' function.
%% 
%% This function should return a list of all data points supported by 
%% the exometer entry implementation. The returned data points shall
%% be supported by the module's `get_value()' function.
%% 
%% 
%% === setopts/4 ===
%% 
%% The `setopts()' function is invoked as follows:
%% 
%% <pre lang="erlang">
%%      setopts(Name, Options, Type, Ref)</pre>
%% 
%% + `Name'
%%     <br/>Specifies the name of the metric to return available datapoints for.
%% 
%% + `Options'
%%     <br/>Specifies an option list that contains additional setup directives to
%%     the entry. The actual options to support are implementation dependent.
%% 
%% + `Type'
%%     <br/>Specifies the type provided to the `exometer:new()' call (before it
%%     was translated by the type - exometer entry map). 
%% 
%% + `Ref'
%%     <br/>Will contain the same tuple returned as `Ref' by the module's
%%     `new()' function.
%% 
%% This function should modify the behavior of the given metric by the
%% options provided in the `Options' property list.
%% 
%% The function should return either `ok' or `{error, Reason}', where
%% `Reason' contins a descriptive reason for a failure to set one or more
%% options.
%%
%% @end
-module(exometer_entry).
-export_type([name/0, type/0, options/0, datapoints/0, value/0, ref/0, error/0]).

-type name()     :: list().
-type type()     :: atom().
-type options()  :: [{atom(), any()}].
-type datapoints()  :: [atom()].
-type value()    :: any().
-type ref()      :: any().
-type error()   :: { error, any() }.


-callback behaviour() -> entry.

-callback new(name(), type(), options()) ->
    ok | {ok, ref()} | error().

-callback delete(name(), type(), ref()) ->
    ok | error().

-callback get_value(name(), type(), ref(), datapoints()) ->
    {ok, value()} | error().

-callback update(name(), value(), type(), ref()) ->
    ok | {ok, value()} | error().

-callback reset(name(), type(), ref()) ->
    ok | {ok, value()} | error().

-callback sample(name(), type(), ref()) ->
    ok | error().

-callback get_datapoints(name(), type(), ref()) ->
    datapoints().

-callback setopts(name(), options(), type(), ref()) ->
    ok | error().
