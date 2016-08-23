-module(calc_handler).
-behaviour(gen_server).

-export([start/3]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {transport, socket, binding}).

start(Transport, Sock, Args) ->
    init([Transport, Sock, Args]).

init([Transport, Sock, _Args]) ->
    Binding = erl_eval:new_bindings(),
    gen_server:enter_loop(?MODULE, [], #state{transport = Transport,
					      socket = Sock,
					      binding = Binding}).

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({tcp, _Port, Data}, #state{transport = Transport,
				       socket = Socket,
				       binding = Binding} = State) ->

    {Result, NewBinding}  = calculate(Data, Binding),
    Transport:send(Socket, Result),
    {noreply, State#state{binding = NewBinding}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


calculate(Request, Binding) ->
    StripedRequest = strip(Request),
    DotEndedRequest = StripedRequest ++ ".",
    {ok, Tokens, _} = erl_scan:string(DotEndedRequest, 0),
    try
	case erl_parse:parse_exprs(Tokens) of
	    {ok, Exprs} ->
		{value, Result, NewBinding} = erl_eval:exprs(Exprs, Binding),
		{format_result(StripedRequest, Result), NewBinding};
	    _ ->
		{"error: can not parse the input!\n", Binding}
	end
    catch
	_Type:_Exception ->
	    {"error: can not evaluate the input!\n", Binding}
    end.

strip(Request0) ->
    Request1 = string:strip(Request0, right, $\n),
    string:strip(Request1, right, $\r).

format_result(Request, Result)
  when is_integer(Result) ->
    Request ++ " = " ++ integer_to_list(Result) ++ "\n";
format_result(Request, Result)
  when is_float(Result) ->
    Request ++ " = " ++ float_to_list(Result) ++ "\n";
format_result(_, _) ->
    "error: can not format the result!\n".
