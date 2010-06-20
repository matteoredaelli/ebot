#!/bin/bash

width=1000
height=800
step=300
#step=172800
#step=28800

rrdpath=../priv/data
rrdfile=email_clients.rrd
outdir=../priv/www

for start in -15m -1hours -1days -7days -31days
do
    rrd="ebot_mq"

#  --lower-limit 1
    /usr/bin/rrdtool graph $outdir/${rrd}${start}.png -a PNG   \
        --start $start --end now --step $step              \
        --title "${rrd} ${start}"       \
        --vertical-label "Totals" --units-length 6 \
        --width $width --height $height --units-exponent 0    \
        DEF:my1=${rrdpath}/${rrd}.rrd:queue_new_0:AVERAGE              \
        DEF:my2=${rrdpath}/${rrd}.rrd:queue_new_1:AVERAGE              \
	DEF:my3=${rrdpath}/${rrd}.rrd:queue_new_2:AVERAGE              \
	DEF:my4=${rrdpath}/${rrd}.rrd:queue_new_3:AVERAGE              \
	DEF:my5=${rrdpath}/${rrd}.rrd:queue_new_4:AVERAGE              \
        LINE1:my1#FF0000:"queue0"	\
        LINE2:my2#0000FF:"queue1"	\
	LINE3:my3#00FFFF:"queue2"	\
	LINE4:my4#00FF00:"queue3"	\
	LINE5:my5#FFFF00:"queue4" 

    rrd="ebot_db"

    /usr/bin/rrdtool graph $outdir/${rrd}${start}.png -a PNG   \
        --start $start --end now --step $step              \
        --title "${rrd} ${start}"       \
        --vertical-label "Totals" --units-length 6 \
        --width $width --height $height --units-exponent 0    \
        DEF:disk=${rrdpath}/${rrd}.rrd:disk_size:AVERAGE              \
        DEF:docs=${rrdpath}/${rrd}.rrd:doc_count:AVERAGE              \
        CDEF:docsK=docs,10000,/              \
        LINE1:disk#FF0000:"Disk size"	\
        LINE2:docsK#0000FF:"Doc count"

    rrd="ebot_crawler"

    /usr/bin/rrdtool graph $outdir/${rrd}${start}.png -a PNG   \
        --start $start --end now --step $step              \
        --title "${rrd} ${start}"       \
        --vertical-label "Totals" --units-length 6 \
        --width $width --height $height --units-exponent 0    \
        DEF:new_urls=${rrdpath}/${rrd}.rrd:new_urls:AVERAGE              \
        DEF:visited_urls=${rrdpath}/${rrd}.rrd:visited_urls:AVERAGE            \
        LINE1:new_urls#FF0000:"new_urls"	\
        LINE2:visited_urls#0000FF:"visited_urls" 


    for rrd in ebot_html ebot_web ; do
	/usr/bin/rrdtool graph $outdir/${rrd}${start}.png -a PNG   \
            --start $start --end now --step $step              \
            --title "${rrd} ${start}"       \
            --vertical-label "Totals" --units-length 6 \
            --width $width --height $height --units-exponent 0    \
            DEF:my1=${rrdpath}/${rrd}.rrd:workers_0:AVERAGE              \
            DEF:my2=${rrdpath}/${rrd}.rrd:workers_1:AVERAGE              \
	    DEF:my3=${rrdpath}/${rrd}.rrd:workers_2:AVERAGE              \
            LINE1:my1#FF0000:"workers_0"	\
            LINE2:my2#0000FF:"workers_1"	\
	    LINE3:my3#00FF00:"workers_2"    
    done
done

