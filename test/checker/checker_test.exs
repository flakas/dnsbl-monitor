defmodule DnsblMonitor.CheckerTest do
  use ExUnit.Case
  doctest DnsblMonitor.Checker
  alias DnsblMonitor.Checker

  describe "listed_on_blacklist/2 when listed on a blacklist" do

    test "returns :listed" do
      assert elem(get_single_listing(), 0) == :listed
    end

    test "contains the resolved listing IP address" do
      assert elem(get_single_listing(), 1) == "127.0.0.126"
    end

    test "contains explanatory messages" do
      assert elem(get_single_listing(), 2) == ["wild.surbl.org permanent test point"]
    end
  end

  describe "listed_on_blacklist/2 when not listed on a blacklist" do
    test "returns :not_listed" do
      assert Checker.listed_on_blacklist("twitter.com", "multi.surbl.org") == :not_listed
    end
  end

  describe "get_explanatory_message/1" do
    test "returns :nxdomain when a message does not exist" do
      assert Checker.get_explanatory_message("test.surbl.org.ph.surbl.org") == :nxdomain
    end

    test "returns messages when they do exist" do
      assert Checker.get_explanatory_message("test.surbl.org.multi.surbl.org") == ["wild.surbl.org permanent test point"]
    end
  end

  describe "prepare_address/1" do
    test "leaves domains unchanged" do
      assert Checker.prepare_address("example.org") == "example.org"
    end

    test "reverses order of IPV4 octets" do
      assert Checker.prepare_address("127.0.0.1") == "1.0.0.127"
    end
  end

  describe "reverse_ipv4_octets/1" do
    test "reverses order of IPV4 octets" do
      assert Checker.reverse_ipv4_octets("127.0.0.1") == "1.0.0.127"
    end
  end

  defp get_single_listing() do
      Checker.listed_on_blacklist("test.surbl.org", "multi.surbl.org")
  end
end

