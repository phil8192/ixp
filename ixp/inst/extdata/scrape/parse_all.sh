#!/bin/bash

RAW="../raw"

function parse {
  for i in $(ls $RAW/*.json) ;do
    python3 parse_ixp.py $1 $i
  done
}

echo "processing lon1..."
parse "lon1" 2>/dev/null |grep -v None 1>../csv/lon1.csv
echo "processing lon2..."
parse "lon2" 2>/dev/null |grep -v None 1>../csv/lon2.csv
echo "processing man1..."
parse "man1" 2>/dev/null |grep -v None 1>../csv/man1.csv
echo "processing sco1..."
parse "sco1" 2>/dev/null |grep -v None 1>../csv/sco1.csv
echo "processing car1..."
parse "car1" 2>/dev/null |grep -v None 1>../csv/car1.csv
echo "processing nva1..."
parse "nva1" 2>/dev/null |grep -v None 1>../csv/nva1.csv
