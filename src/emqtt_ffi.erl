-module(emqtt_ffi).

-export([start_link/1, connect/1, stop/1, subscribe/2]).

start_link(Options) ->
  emqtt:start_link(Options).

connect(ConnPid) ->
  emqtt:connect(ConnPid).

subscribe(ConnPid, Topic) ->
  SubOpts = [{qos, 1}],

  case emqtt:subscribe(ConnPid, #{}, [{Topic, SubOpts}]) of
    {ok, Properties, ReasonCodes} ->
      {ok, {Properties, ReasonCodes}};

    {error, Error} ->
      {error, Error}
  end.

stop(ConnPid) ->
  normalize(emqtt:stop(ConnPid)).

normalize(ok) -> {ok, nil};
normalize({ok, T}) -> {ok, T}.
