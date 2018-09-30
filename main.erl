-module(main).
-compile(export_all).

-import(checker, [is_blacklisted/1]).

check_all() ->
  case file:read_file("ips.txt") of
    {ok, Contents} ->
      IPs = string:split(string:trim(binary:bin_to_list(Contents)), "\n", all),
      [{IP, checker:is_blacklisted(IP)} || IP <- IPs];
    _ -> error
  end.
