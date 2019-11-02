# Read in json obtained from snatch.sh, write out a csv with the following form:
#
# ts, lon1, lon2, man1, sco1, car1, nva1
#
# Where ts = timestamp (Seconds since epoch/UTC).
#            1st ts = 00:00:00
#            Last   = 23:55:00
# lon1,.., = One of the 6 IXPs belonging to the LINX network.
import sys
import json
import csv

# <ixp> <json>
ixp, f = sys.argv[1:]

with open(f) as json_f:
    d = json.load(json_f)

throughput = d["throughput"]

rates = [(int(ts*0.001), rate) for ts, rate in throughput[ixp]["bitrate"]["in"]]
for ts, rate in rates:
    print("{},{}".format(ts, rate))
