#!/bin/bash
#
# Download LINX IXP bandwidth use data
# ====================================
#
# LINX maintain a bandwidth statistics page:
#
# https://portal.linx.net/
#
# backed by a JSON/REST API which exposes bandwidth use for
# various time periods aggregated by 5 minute, hourly and daily buckets.
#
# The maximum lookback period for 5 minute buckets is 24 hours, however the API
# accepts 2 parameters, start and end. start can be set to 0, however the 
# returned data will be automatically aggregated to daily buckets.
#
# By setting both start and end to be a 24 hour window, it is possible to move
# this window backwards and obtain higher granularity data for 5 minute 
# intervals.
#
# This script accepts as an argument a single parameter <backfill days>. For 
# each day, starting with yesterday (exluding today), the script will download
# 5 minute bandwidth data in 24 hour time windows.
#
# Note that the data seem to start from 2015-06-15.
#
# Note that is does not work on a mac.

URL="https://portal.linx.net/api/lans/throughput"
OUT="../raw"

mkdir -p $OUT

if [ $# == 0 ] ;then echo $0 "<backfill days>" ;exit 0 ;fi 
backfill_days=$1
today=$(date --utc --date=$(date "+%Y-%m-%d") "+%s")
yest_1_sec_b4_mid=$(($today - 1))

mkdir -p $OUT
for ((i=$backfill_days,
      from=$((yest_1_sec_b4_mid - 86399)),
      to=$yest_1_sec_b4_mid;
      --i>=0;
      from-=86400,
      to-=86400)) ;do
  file="$OUT/$(date +"%Y-%m-%d" --utc -d @$from).json"
  if [ -f $file ] ;then echo "skipping $file" ;continue ;fi
  echo $(date +"%Y-%m-%d %k:%M:%S" --utc -d @$from) ">" \
       $(date +"%Y-%m-%d %k:%M:%S" --utc -d @$to) ">" $file
  curl -s "$URL?start=${from}000&end=${to}000" >$file
done
