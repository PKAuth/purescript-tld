tld
===

This project separates subdomains, domains, and top-level-domains from URLs in PureScript. 
It is a port of the Haskell library](https://hackage.haskell.org/package/tld). 
This list comes from `http://mxr.mozilla.org/mozilla/source/netwerk/dns/src/effective_tld_names.dat?raw=1`.

You can manually update the TLD list by running the following:
    wget http://mxr.mozilla.org/mozilla/source/netwerk/dns/src/effective_tld_names.dat?raw=1 -O data/tld.dat
    ./process.py
    pulp build
