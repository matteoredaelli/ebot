#!/bin/bash

width=1000
height=800
step=300
#step=172800
#step=28800

rrdpath=../priv/data
rrdfile=email_clients.rrd
outdir=../priv/www

for start in -1hours -1days -7days -31days
do
    rrd="ebot_amqp"
#  --lower-limit 1
    /usr/bin/rrdtool graph $outdir/${rrd}${start}.png -a PNG   \
        --start $start --end now --step $step              \
        --title "${rrd} ~ ${start}"       \
        --vertical-label "Totals" --units-length 6 \
        --width $width --height $height --units-exponent 0    \
        DEF:my0=${rrdpath}/${rrd}.rrd:queue_new_0:AVERAGE              \
        DEF:my1=${rrdpath}/${rrd}.rrd:queue_new_1:AVERAGE              \
	DEF:my2=${rrdpath}/${rrd}.rrd:queue_new_2:AVERAGE              \
	DEF:my3=${rrdpath}/${rrd}.rrd:queue_new_3:AVERAGE              \
	DEF:my4=${rrdpath}/${rrd}.rrd:queue_new_4:AVERAGE              \
        LINE0:my0#FF0000:"queue0"	\
        LINE1:my1#0000FF:"queue1"	\
	LINE2:my2#00FFFF:"queue2"	\
	LINE3:my3#00FF00:"queue3"	\
	LINE4:my4#00FF00:"queue4" 
    rrd="ebot_db"
#  --lower-limit 1
    /usr/bin/rrdtool graph $outdir/${rrd}${start}.png -a PNG   \
        --start $start --end now --step $step              \
        --title "${rrd} ~ ${start}"       \
        --vertical-label "Totals" --units-length 6 \
        --width $width --height $height --units-exponent 0    \
        DEF:my0=${rrdpath}/${rrd}.rrd:disk_size:AVERAGE              \
        DEF:my1=${rrdpath}/${rrd}.rrd:doc_count:AVERAGE              \
        LINE1:my0#FF0000:"Disk size"	\
        LINE2:my1#0000FF:"Doc count"
done

