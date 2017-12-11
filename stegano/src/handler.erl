-module(handler).

-export([init/2]).
-import_lib(stegano).

init(Req0, Opts) ->
	Method = cowboy_req:method(Req0),
	Req = handle(Method, Req0),
	{ok, Req, Opts}.

handle(<<"GET">>, Req) ->
	#{text := Text} = cowboy_req:match_qs([{text, [], undefined}], Req),
	#{imagename:= ImageName} = cowboy_req:match_qs([{imagename, [], undefined}], Req),
	case {Text, ImageName} of
		{undefined, _} -> cowboy_req:reply(400, #{}, <<"Missing 'text' parameter.">>, Req);
		{_, undefined} -> cowboy_req:reply(400, #{}, <<"Missing 'imagename' parameter.">>, Req);
		{_, _} ->
			Bin = stegano:cipher(ImageName, Text, 2),
			Req1 = cowboy_req:set_resp_header(<<"content-disposition">>, "attachment; filename=ciphered.bmp", Req),
			Req2 = cowboy_req:set_resp_body(Bin, Req1),
			cowboy_req:reply(200, #{
				<<"content-type">> => <<"application/octet-stream">>
			}, Req2)
	end;
handle(<<"PUT">>, Req) ->
	{ok, Headers, Req2} = cowboy_req:read_part(Req),
	{ok, Data, _} = cowboy_req:read_part_body(Req2),
	{file, <<"data">>, Filename, ContentType} = cow_multipart:form_data(Headers),
	io:format("Received file ~p of content-type ~p as follow:~n~p~n~n", [Filename, ContentType, Data]),
	{ok, File} = file:open(Filename, [write]),
	file:write(File, Data),
	cowboy_req:reply(200, #{
		<<"content-type">> => <<"text/plain; charset=utf-8">>
	}, <<"OK">>, Req);
handle(<<"DELETE">>, Req) ->
	{ok, Filename, _} = cowboy_req:read_body(Req),
  io:format("Deleting file ~p", [Filename]),
	file:delete(Filename),
	cowboy_req:reply(200, #{
		<<"content-type">> => <<"text/plain; charset=utf-8">>
	}, <<"OK">>, Req);
handle(_, Req) ->
	cowboy_req:reply(405, Req).