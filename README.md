# DNSBL Monitor

This is an application whose sole intention is to monitor whether or not a set of given IPs can currently be found on a set of given blacklists.

### How it works

[How to query DNSBL](https://en.wikipedia.org/wiki/DNSBL#DNSBL_queries)

For an IP address of 12.23.34.45:
1. Reverse order of octets: `12.23.34.45` -> `45.34.23.12`
2. Append DNSBL URI: `45.34.23.12.zen.spamhaus.org`
3. Query your recursive DNS for A record of this URI, which will either result in an address if the IP address is listed, or `NXDOMAIN` if it is not
4. For listed IPs query TXT record for the URI to find the explanatory message: `https://www.spamhaus.org/sbl/query/SBL123456`
