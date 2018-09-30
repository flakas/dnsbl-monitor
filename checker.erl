-module(checker).
-compile(export_all).

-include_lib("kernel/src/inet_res.hrl").

blacklists() ->
  #{
    spamhaus => "zen.spamhaus.org",
    sorbs => "dnsbl.sorbs.net"
  }.

is_blacklisted(IP) ->
  [{Blacklist, listed_on_blacklist(IP, Hostname)} || {Blacklist, Hostname} <- maps:to_list(blacklists())].

listed_on_blacklist(IP, Blacklist) ->
  Hostname = reverse_ip_octets(IP) ++ "." ++ Blacklist,
  Query = inet_res:getbyname(Hostname, a),
  case Query of
       {ok, _} -> listed;
       {error, _} -> not_listed
  end.

reverse_ip_octets(IP) ->
  string:join(lists:reverse(string:split(IP, ".", all)), ".").
