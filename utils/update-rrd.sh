http_proxy=
while [ 1 ]; do
	curl http://localhost:8000/worker/web/check_recover
	curl http://localhost:8000/worker/html/check_recover
	curl http://localhost:8000/stats/update_rrd
	sleep 300	
done
