defmodule DnsblMonitor.Checker do

  def blacklists() do
    %{
      :spamhaus => "zen.spamhaus.org",
      :sorbs => "dnsbl.sorbs.net",
      :sorbs_spam => "spam.dnsbl.sorbs.net",
      :barracuda => "b.barracudacentral.org",
      :spamrats => "spam.spamrats.com",
      :spamcop => "bl.spamcop.net",
      :msrbl => "combined.rbl.msrbl.net",
      :ascams => "block.ascams.com",
      :ascams_super => "superblock.ascams.com",
      :manitu => "ix.dnsbl.manitu.net",
      :apews => "l2.apews.org",
      :nordspam => "bl.nordspam.com",
      :ratsall => "all.spamrats.com",
      :surbl_multi => "multi.surbl.org",
      :sem_black => "bl.spameatingmonkey.net",
      :sem_netblack => "netbl.spameatingmonkey.net",
      :unsubscribe_blacklist => "ubl.unsubscore.com"
    }
  end

  def is_blacklisted(ip) do
    for {blacklist, hostname} <- blacklists(), do: {blacklist, listed_on_blacklist(ip, hostname)}
  end

  def listed_on_blacklist(ip, blacklist) do
    hostname = reverse_ip_octets(ip) <> "." <> blacklist
    query = :inet_res.getbyname(to_charlist(hostname), :a)
    case query do
      {:ok, _} -> {:listed, get_explanatory_message(hostname)}
      {:error, _} -> :not_listed
    end
  end

  def reverse_ip_octets(ip) do
    ip
    |> String.split(".")
    |> Enum.reverse()
    |> Enum.join(".")
  end

  def get_explanatory_message(hostname) do
    query = :inet_res.getbyname(to_charlist(hostname), :txt)
    case query do
      {:ok, {:hostent, _, _, _, _, messages}} ->
        messages
        |> Enum.map(fn msg -> List.flatten(msg) |> to_string end)
      {:error, err} ->
        err
    end
  end

end
