defmodule Mix.Tasks.Check do
  use Mix.Task

  def run([]), do: IO.puts("Usage: mix check <path>")
  def run([filename]) do
    case DnsblMonitor.show_all(filename) do
      :ok -> :ok
      {:error, reason} -> IO.puts("Error: #{reason}")
    end
  end
end
