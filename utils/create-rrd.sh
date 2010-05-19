NOW=`date +%s`
ONE_WEEK_AGO=$(($NOW-604800))
TARGET_DIR=../priv/data

rrdtool create $TARGET_DIR/ebot_amqp.rrd --start $NOW  --step 300        \
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

rrdtool create $TARGET_DIR/ebot_web.rrd --start $NOW  --step 300	\
    DS:crawlers_0:GAUGE:600:0:U	\
    DS:crawlers_1:GAUGE:600:0:U	\
    DS:crawlers_2:GAUGE:600:0:U	\
    RRA:AVERAGE:0.5:1:1008	\
    RRA:AVERAGE:0.5:12:744	\
    RRA:AVERAGE:0.5:288:365 
