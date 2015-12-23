#!/usr/bin/env python

import string

dataf = "./data/tld.dat"
modulef = "./src/Network/URI/TLD/Internal.purs"

with open(dataf) as f:
				tlds = f.readlines()

# Strip newlines.
tlds = map(lambda tld : tld.strip(), tlds)

# Skip empty lines and lines with non-alphanumeric characters.
tlds = filter(lambda tld : tld != "" and set(tld) <= set(string.ascii_lowercase + string.digits + '.'), tlds)

# Write internal module file.
with open(modulef, "w+") as f:
				f.write('module Network.URI.TLD.Internal where\n')
				f.write('\n')
				f.write('import qualified Data.List as List\n')
				f.write('import Data.Set (Set())\n')
				f.write('import qualified Data.Set as Set\n')
				f.write('import Prelude\n')
				f.write('\n')
				f.write('tldSet :: Unit -> Set String\n')
				f.write('tldSet _ = Set.fromList $ List.toList [\n')
				f.write('      "localhost"\n')
				map(lambda tld : f.write('    , "' + tld + '"\n') , tlds)
				f.write('    ]\n')
				f.write('\n')

# print( tlds)
