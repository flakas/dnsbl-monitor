defmodule DnsblMonitor do
  def show_all(addresses_file, blacklists_file) when is_bitstring(addresses_file) and is_bitstring(blacklists_file) do
    with {:ok, blacklists} <- read_lines(blacklists_file),
         {:ok, addresses} <- read_lines(addresses_file) do
      show_all(addresses, blacklists)
    end
  end

  def show_all(addresses, blacklists) when is_list(addresses) and is_list(blacklists) do
    DnsblMonitor.Checker.are_listed_on_blacklists(addresses, blacklists)
    |> Stream.filter(fn {_address, _blacklist, listing} -> listing != :not_listed end)
    |> Enum.each(fn {address, blacklist, listing} -> show_address_listing(address, blacklist, listing) end)
  end

  def read_lines(path) do
    case File.read(path) do
      {:ok, contents} -> {:ok, String.split(contents, "\n", trim: true)}
      {:error, reason} -> {:error, reason}
    end
  end

  def show_address_listing(address, blacklist, {:listed, ip, messages}) do
    case messages do
      :nxdomain -> IO.puts("#{address}: #{blacklist} -> #{ip}")
      messages ->
        for message <- messages do
          IO.puts("#{address}: #{blacklist} -> #{ip} (#{message})")
        end
    end
  end
end
