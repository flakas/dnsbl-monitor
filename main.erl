-module(main).
-export([show_all/2, get_all/2, check_ip_list/1]).

-import(checker, [is_blacklisted/1]).

show_all(File, Workers) ->
  AllListings = lists:keysort(1, get_all(File, Workers)),
  lists:foreach(fun ({IP, Listings}) ->
                    show_ip_listings(IP, Listings)
                end,
                AllListings).

show_ip_listings(_IP, []) ->
  ok;
show_ip_listings(IP, [{_, not_listed}|Rest]) ->
  show_ip_listings(IP, Rest);
show_ip_listings(IP, [{Blacklist, {listed, Messages}}|Rest]) ->
  io:fwrite("~s: ~s (~s)~n", [IP, Blacklist, case is_list(Messages) of
                                               true -> lists:flatten(Messages);
                                               false -> unknown
                                             end]),
  show_ip_listings(IP, Rest).

get_all(File, Workers) ->
  case file:read_file(File) of
    {ok, Contents} ->
      IPs = string:split(string:trim(binary:bin_to_list(Contents)), "\n", all),
      check_ips(IPs, Workers);
    _ -> error
  end.

check_ips(IPs, Workers) ->
  Chunks = split_list_into_chunks(IPs, Workers),
  [spawn(?MODULE, check_ip_list, [{self(), Chunk}]) || Chunk <- Chunks],
  Answers = get_answers(Workers, []),
  lists:flatten(Answers).

split_list_into_chunks(List, Chunks) ->
  Indexed = lists:zip(lists:seq(1, length(List)), List),
  [lists:filtermap(fun({Index, E}) ->
                       case Index rem Chunks + 1 == Nth of
                         true -> {true, E};
                         false -> false
                       end
                   end,
                   Indexed)
   || Nth <- lists:seq(1, Chunks)].

get_answers(0, Answers) ->
  Answers;
get_answers(Remaining, Answers) when Remaining > 0 ->
  receive
    {ok, Answer} ->
      get_answers(Remaining - 1, [Answer | Answers]);
    {error, _} -> get_answers(Remaining - 1, Answers)
  end.

check_ip_list({From, IPs}) ->
  check_ip_list({From, IPs, []});
check_ip_list({From, [], Results}) ->
  From ! {ok, Results};
check_ip_list({From, [IP|IPs], Results}) ->
  Result = checker:is_blacklisted(IP),
  check_ip_list({From, IPs, [{IP, Result}|Results]}).
