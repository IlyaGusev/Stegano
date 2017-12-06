-module(get_handler).
-behavior(cowboy_handler).

-export([init/2]).
-import_lib(stegano).

init(Req0, Opts) ->
	Method = cowboy_req:method(Req0),
	#{echo := Text} = cowboy_req:match_qs([{echo, [], undefined}], Req0),
	Req = handle_text(Method, Text, Req0),
	{ok, Req, Opts}.

handle_text(<<"GET">>, undefined, Req) ->
	cowboy_req:reply(400, #{}, <<"Missing text parameter.">>, Req);
%%handle_text(<<"GET">>, Text, Req) ->
%%	Bin = stegano:cipher("/home/yallen/Erlang/web/lenna.bmp", "/home/yallen/Erlang/web/lenna_c.bmp", Text, 8),
%%	cowboy_req:reply(200, #{
%%		<<"content-type">> => <<"text/plain; charset=utf-8">>
%%	}, Bin, Req);

handle_text(<<"GET">>, Text, Req) ->
	Bin = stegano:cipher("/home/yallen/Erlang/web/lenna.bmp", "/home/yallen/Erlang/web/lenna_c.bmp", Text, 8),
	Req1 = cowboy_req:set_resp_header(<<"content-disposition">>, "attachment; filename=lenna_c.bmp", Req),
	Req2 = cowboy_req:set_resp_body(Bin, Req1),
	cowboy_req:reply(200, #{
		<<"content-type">> => <<"application/octet-stream">>
	}, Req2);
handle_text(_, _, Req) ->
	cowboy_req:reply(405, Req).