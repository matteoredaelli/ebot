http_proxy=
while [ 1 ]; do
	curl http://localhost:8000/stats/update_rrd
	sleep 300	
done
