-module(main).
-compile(export_all).

-include_lib("kernel/src/inet_res.hrl").

blacklists() ->
  #{
    spamhaus => "zen.spamhaus.org",
    sorbs => "dnsbl.sorbs.net"
  }.

is_blacklisted(IP) ->
  [{Blacklist, listed_on_blacklist(IP, URL)} || {Blacklist, URL} <- maps:to_list(blacklists())].

listed_on_blacklist(IP, Blacklist) ->
  URL = reverse_ip(IP) ++ "." ++ Blacklist,
  Query = inet_res:getbyname(URL, a),
  case Query of
       {ok, _} -> listed;
       {error, _} -> not_listed
  end.

reverse_ip(IP) ->
  string:join(lists:reverse(string:split(IP, ".", all)), ".").

