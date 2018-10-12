-module(main).
-export(check_all/2).

-import(checker, [is_blacklisted/1]).

check_all(File, Workers) ->
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
