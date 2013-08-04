exometer
========

Basic measurement objects and probe behavior

## Application structure

The application exometer creates a set of ETS tables at startup:

* One shared set table: `exometer_shared`
* One set table for each scheduler: `exometer_N` (where `N` is the sched ID)

This is done by the generic server `exometer_admin`, which currently doesn't
have anything else to do. (Actually, the ets tables are started before
the exometer_admin starts, so that they are owned by the supervisor,
but the code is found in `exometer_admin.erl`).


## `exometer_ctr.erl`

A basic counter implementation, using the scheduler-specific ETS tables for best performance.

## `exometer_probe.erl`

The `exometer_probe` module implements a basic probe behavior.

The callback module needs to implement the following functions:

* init(Name, Options, Opaque) -> {ok, ModState} | {error, Reason}
* sample(ModSt, Opaque) -> {ok, ModSt1} | {ok, [Event], ModSt1} | {error, Reason}
* get_value(ModSt, Opaque) -> {ok, Value} | {ok, Value, ModSt1}
* event(Event, ModSt) -> {ok, ModSt1} | {ok, [Event1], ModSt1} | {error, Reason}
* code_change(FromVsn, ModSt, Extra) -> {ok, ModSt1}

The following options are supported:

* {sample_interval, infinity | integer()} - for sampling, using the Mod:sample/2 callback
* {report_interval, infinity | integer()} - for periodic reporting to subscribers
* {user, Opts} - options passed to the callback module

## `exometer_probe_ctr.erl`

An example probe, sampling a set of exometer counters. See `exometer_probe_ctr:test()` for example.
