NOW=`date +%s`
ONE_WEEK_AGO=$(($NOW-604800))
TARGET_DIR=../priv/data

rrdtool create $TARGET_DIR/ebot_mq.rrd --start $NOW  --step 300        \
    DS:queue_new_0:GAUGE:600:0:U  \
    DS:queue_new_1:GAUGE:600:0:U  \
    DS:queue_new_2:GAUGE:600:0:U  \
    DS:queue_new_3:GAUGE:600:0:U  \
    DS:queue_new_4:GAUGE:600:0:U  \
    RRA:AVERAGE:0.5:1:1008       \
    RRA:AVERAGE:0.5:12:744       \
    RRA:AVERAGE:0.5:288:365

rrdtool create $TARGET_DIR/ebot_db.rrd --start $NOW  --step 300	\
    DS:disk_size:GAUGE:600:0:U	\
    DS:doc_count:GAUGE:600:0:U	\
    RRA:AVERAGE:0.5:1:1008	\
    RRA:AVERAGE:0.5:12:744	\
    RRA:AVERAGE:0.5:288:365 

rrdtool create $TARGET_DIR/ebot_crawler.rrd --start $NOW  --step 300	\
    DS:new_urls:GAUGE:600:0:U	\
    DS:visited_urls:GAUGE:600:0:U   \
    RRA:AVERAGE:0.5:1:1008	\
    RRA:AVERAGE:0.5:12:744	\
    RRA:AVERAGE:0.5:288:365 

for rrd in ebot_html ebot_web ; do
    rrdtool create $TARGET_DIR/${rrd}.rrd --start $NOW  --step 300	\
	DS:workers_0:GAUGE:600:0:U	\
	DS:workers_1:GAUGE:600:0:U	\
	DS:workers_2:GAUGE:600:0:U	\
	DS:workers_3:GAUGE:600:0:U	\
	RRA:AVERAGE:0.5:1:1008	\
	RRA:AVERAGE:0.5:12:744	\
	RRA:AVERAGE:0.5:288:365 
done