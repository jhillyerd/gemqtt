-module(emqtt_ffi).

-export([
  connect/1, decode_client/1, disconnect/1, publish/5, start_link/1, stop/1,
  subscribe/3, unsubscribe/2
]).

start_link(Options) ->
  normalize(emqtt:start_link(Options)).

connect(Client) ->
  { client, ConnPid } = Client,
  normalize(emqtt:connect(ConnPid)).

disconnect(Client) ->
  { client, ConnPid } = Client,
  try normalize(emqtt:disconnect(ConnPid))
  catch exit:{noproc, _} -> {error, noproc}
  end.

subscribe(Client, SubOpts, Topics) ->
  { client, ConnPid } = Client,
  % Pair each topic with the provided SubOpts.
  TopicPairs = lists:map(fun(T) -> {T, SubOpts} end, Topics),
  case emqtt:subscribe(ConnPid, #{}, TopicPairs) of
    {ok, undefined, Reasons} -> {ok, {none, Reasons}};
    Other -> normalize(Other)
  end.

unsubscribe(Client, Topics) ->
  { client, ConnPid } = Client,
  normalize(emqtt:unsubscribe(ConnPid, #{}, Topics)).

publish(Client, Topic, Props, Payload, Opts) ->
  { client, ConnPid } = Client,
  normalize_option(emqtt:publish(ConnPid, Topic, Props, Payload, Opts)).

stop(Client) ->
  { client, ConnPid } = Client,
  try normalize(emqtt:stop(ConnPid))
  catch exit:{noproc, _} -> {error, noproc}
  end.

% Normalize emqtt return values for Result(t, e).
normalize(ok) -> {ok, nil};
normalize({ok, undefined}) -> {ok, nil};
normalize({ok, Pid}) when is_pid(Pid) -> {ok, {client, Pid}};
normalize({ok, T}) -> {ok, T};
normalize({ok, T, U}) -> {ok, {T, U}};
normalize({error, T}) -> {error, T}.

% Normalize emqtt return values for Result(Option(t), e).
normalize_option(ok) -> {ok, none};
normalize_option({ok, T}) -> {ok, {some, T}};
normalize_option({error, T}) -> {error, T}.

% For decoding Client from a Pid in messsages.
decode_client(Pid) when is_pid(Pid) -> {ok, {client, Pid}};
decode_client(nil) -> decode_client_error(<<"Nil">>);
decode_client(_) -> decode_client_error(<<"Some other type">>).

decode_client_error(Got) ->
  {error, [{decode_error, "Pid", Got, []}]}.
