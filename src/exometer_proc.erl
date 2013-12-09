-module(exometer_proc).

-export([spawn_process/2,
	 cast/2,
	 call/2,
	 process_options/1]).

spawn_process(Name, F) when is_function(F,0) ->
    proc_lib:spawn(fun() ->
			   exometer_admin:monitor(Name, self()),
			   F()
		   end).

cast(Pid, Msg) ->
    Pid ! {exometer_proc, Msg},
    ok.

call(Pid, Req) ->
    MRef = erlang:monitor(process, Pid),
    Pid ! {exometer_proc, {self(), MRef}, Req},
    receive
	{MRef, Reply} ->
	    Reply;
	{'DOWN', MRef, _, _, Reason} ->
	    error(Reason)
    after 5000 ->
	    error(timeout)
    end.

stop() ->
    exometer_admin:demonitor(self()),
    exit(normal).

process_options(Opts) ->
    lists:foreach(
      fun({priority, P}) when P==low; P==normal; P==high; P==max ->
	      process_flag(priority, P);
	 ({min_heap_size, S}) when is_integer(S), S > 0 ->
	      process_flag(min_heap_size, S);
	 ({min_vheap_size, S}) when is_integer(S), S > 0 ->
	      process_flag(min_vheap_size, S);
	 ({sensitive, B}) when is_boolean(B) ->
	      process_flag(sensitive, B);
	 ({scheduler, I}) when is_integer(I), I >= 0 ->
	      process_flag(scheduler, I);
	 (_) ->
	      ok
      end, Opts).
