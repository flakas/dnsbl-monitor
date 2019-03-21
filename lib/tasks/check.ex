defmodule Mix.Tasks.Check do
  use Mix.Task

  def run([]), do: IO.puts("Usage: mix check <addresses-path> <blacklists-path>")
  def run([addresses_path, blacklists_path]) do
    case DnsblMonitor.show_all(addresses_path, blacklists_path) do
      :ok -> :ok
      {:error, reason} -> IO.puts("Error: #{reason}")
    end
  end
end

