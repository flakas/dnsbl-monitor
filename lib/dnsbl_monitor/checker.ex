defmodule DnsblMonitor.Checker do

  def are_listed_on_blacklists(addresses, blacklists, timeout \\ 10000) do
    Stream.flat_map(addresses, fn address -> Stream.map(blacklists, &{address, &1}) end)
    |> Task.async_stream(fn {address, blacklist} -> {address, blacklist, listed_on_blacklist(address, blacklist)} end, timeout: timeout, max_concurrency: System.schedulers_online())
    |> Stream.map(fn {:ok, listings} -> listings end)
  end

  def listed_on_blacklist(address, blacklist) do
    hostname = prepare_address(address) <> "." <> blacklist
    query = :inet_res.getbyname(to_charlist(hostname), :a)
    case query do
      {:ok, {:hostent, _, _, _, _, ips}} -> {:listed, to_string(:inet.ntoa(hd(ips))), get_explanatory_message(hostname)}
      {:error, _} -> :not_listed
    end
  end

  def prepare_address(address) do
    case :inet.parse_ipv4_address(to_charlist(address)) do
      {:ok, address} -> reverse_ipv4_octets(to_string(:inet.ntoa(address)))
      {:error, _} -> address
    end
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

  def reverse_ipv4_octets(ip) do
    ip
    |> String.split(".")
    |> Enum.reverse()
    |> Enum.join(".")
  end

end
