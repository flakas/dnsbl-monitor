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

  def check_ips(ips) do
    ips
    |> Enum.map(fn ip -> {ip, Task.async(fn -> DnsblMonitor.Checker.is_blacklisted(ip) end)} end)
    |> Enum.map(fn {ip, task} -> {ip, Task.await(task, 10000)} end)
  end

end
