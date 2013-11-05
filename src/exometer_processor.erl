-module(exometer_processor).
-export_type([name/0, type/0, status/0, options/0, datapoints/0, value/0, ref/0, error/0]).

-type name()     :: list().
-type type()     :: atom().
-type status()   :: enabled | disabled.
-type options()  :: [{atom(), any()}].
-type datapoints()  :: [atom()].
-type value()    :: any().
-type ref()      :: pid() | undefined.
-type error()   :: { error, any() }.

-callback new(name(), type(), options()) ->
    ok | {ok, pid()} | error().
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


