defmodule DnsblMonitor do
  def show_all(file) do
    case read_all_ips(file) do
      {:ok, ips} ->
        ips
        |> check_ips
        |> Enum.each(fn {ip, listings} -> show_ip_listings(ip, listings) end)

        :ok
      {:error, reason} -> {:error, reason}
    end
  end

  def show_ip_listings(_ip, []), do: :ok
  def show_ip_listings(ip, [{_, :not_listed} | rest]), do: show_ip_listings(ip, rest)
  def show_ip_listings(ip, [{blacklist, {:listed, messages}}|rest]) do
    IO.puts("#{ip}: #{blacklist} (#{List.flatten(messages)})")
    show_ip_listings(ip, rest)
  end

  def read_all_ips(file) do
    case File.read(file) do
      {:ok, contents} -> {:ok, String.split(contents, "\n", trim: true)}
      {:error, reason} -> {:error, reason}
    end
  end

  def check_ips(ips, timeout \\ 10000) do
    ips
    |> Task.async_stream(fn ip -> {ip, DnsblMonitor.Checker.is_blacklisted(ip)} end, timeout: timeout)
    |> Enum.map(fn {:ok, listings} -> listings end)
  end

end
