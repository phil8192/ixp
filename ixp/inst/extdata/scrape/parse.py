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

# <ixp> <json> <csv>
ixp, f, out = sys.argv[1:]

with open(f) as json_f:
    d = json.load(json_f)

if d is None or not "throughput" in d.keys():
    print("no throughput", file=sys.stderr)
    sys.exit(1)

throughput = d["throughput"]
keys = ["lon1", "lon2", "man1", "sco1", "car1", "nva1"]

if throughput == [] or set(keys) != set(throughput.keys()):
    print("missing keys: {}".format(list(throughput.keys())), file=sys.stderr)
    sys.exit(1)

ts = [int(ts*0.001) for ts, _ in throughput["lon1"]["bitrate"]["in"]]

# 24*12 = 288 5 minute buckets.
# 24*60*60 - 5*60 seconds in 24 hour period (ts = last 5 minutes)
if 288 != len(ts):
    print("invalid ts length ({})".format(len(ts)), file=sys.stderr)
    sys.exit(1)

assert 86100 == ts[-1] - ts[0]

br = [[rate for _, rate in throughput[k]["bitrate"]["in"]] for k in keys]
br = zip(ts, *br)

with open(out, "w") as csv_f:
    csv_w = csv.writer(csv_f,  delimiter=",")
    csv_w.writerows(br)
