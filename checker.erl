-module(checker).
-export([is_blacklisted/1]).

-include_lib("kernel/src/inet_res.hrl").

blacklists() ->
  #{
    spamhaus => "zen.spamhaus.org",
    sorbs => "dnsbl.sorbs.net",
    sorbs_spam => "spam.dnsbl.sorbs.net",
    barracuda => "b.barracudacentral.org",
    spamrats => "spam.spamrats.com",
    spamcop => "bl.spamcop.net",
    msrbl => "combined.rbl.msrbl.net",
    ascams => "block.ascams.com",
    ascams_super => "superblock.ascams.com",
    manitu => "ix.dnsbl.manitu.net"
  }.

is_blacklisted(IP) ->
  [{Blacklist, listed_on_blacklist(IP, Hostname)} || {Blacklist, Hostname} <- maps:to_list(blacklists())].

listed_on_blacklist(IP, Blacklist) ->
  Hostname = reverse_ip_octets(IP) ++ "." ++ Blacklist,
  Query = inet_res:getbyname(Hostname, a),
  case Query of
       {ok, _} -> {listed, get_explanatory_message(Hostname)};
       {error, _} -> not_listed
  end.

reverse_ip_octets(IP) ->
  string:join(lists:reverse(string:split(IP, ".", all)), ".").

get_explanatory_message(Hostname) ->
  Query = inet_res:getbyname(Hostname, txt),
  case Query of
    {ok, {hostent, _, _, _, _, Messages}} -> Messages;
    {error, _} -> unknown
  end.
