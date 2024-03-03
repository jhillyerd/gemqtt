-module(emqtt_ffi).

-export([
  connect/1, decode_pid/1, disconnect/1, publish/3, start_link/1, stop/1,
  subscribe/2, unsubscribe/2
]).

start_link(Options) ->
  { options, OptMap } = Options,
  emqtt:start_link(OptMap).

connect(ConnPid) ->
  normalize(emqtt:connect(ConnPid)).

disconnect(ConnPid) ->
  normalize(emqtt:disconnect(ConnPid)).

subscribe(ConnPid, Topic) ->
  SubOpts = [{qos, 1}],
  normalize(emqtt:subscribe(ConnPid, #{}, [{Topic, SubOpts}])).

unsubscribe(ConnPid, Topics) ->
  normalize(emqtt:unsubscribe(ConnPid, #{}, Topics)).

publish(ConnPid, Topic, Payload) ->
  normalize(emqtt:publish(ConnPid, Topic, Payload)).

stop(ConnPid) ->
  normalize(emqtt:stop(ConnPid)).

% Normalize emqtt return values for Result(t, e).
normalize(ok) -> {ok, nil};
normalize({ok, undefined}) -> {ok, nil};
normalize({ok, T}) -> {ok, T};
normalize({ok, T, U}) -> {ok, {T, U}};
normalize({error, T}) -> {error, T}.

% For decoding Pid in messsages.
decode_pid(Data) when is_pid(Data) -> {ok, Data};
decode_pid(nil) -> decode_pid_error(<<"Nil">>);
decode_pid(_) -> decode_pid_error(<<"Some other type">>).

decode_pid_error(Got) ->
  {error, [{decode_error, "Pid", Got, []}]}.
