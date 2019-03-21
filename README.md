# DNSBL Monitor

This is an application whose sole intention is to check whether or not a set of given IPs or domains can currently be found on a set of given blacklists.

This was done to learn more about Erlang/Elixir and how blacklists work, and it's not really inteded for anyone to use it.

### How it works

[How to query DNSBL](https://en.wikipedia.org/wiki/DNSBL#DNSBL_queries)

For an IP address of 12.23.34.45:
1. Reverse order of octets: `12.23.34.45` -> `45.34.23.12`
2. Append DNSBL URI: `45.34.23.12.zen.spamhaus.org`
3. Query your recursive DNS for A record of this URI, which will either result in an address if the IP address is listed, or `NXDOMAIN` if it is not
4. For listed IPs query TXT record for the URI to find the explanatory message: `https://www.spamhaus.org/sbl/query/SBL123456`

For a domain of test.surbl.org:
1. Append the DNSBL URI: `test.surbl.org.multi.surbl.org`
3. Query your recursive DNS for A record of this URI, which will either resolve to an IP address if the address is listed, or `NXDOMAIN` if it is not
4. For listed addresses, query the TXT record for the URI to find the explanatory message: `wild.surbl.org permanent test point`

### How to use it
If you do decide to use it:

- Create a file with one IPv4 address or domain per line
- Create a file with one blacklist address per line
- Run `mix check <addresses-path> <blacklists-path>`
