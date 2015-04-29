
"""
commands = get,set,ping,(mset,mget if finished)
n = 1.000.000-5.000.000-30.000.000
c = 1 - 10 - 100 - 1000 - 10000
r = 100000
pipeline = yes/no
"""

ip = "192.168.3.11"

tests = {'ping','get','set','mset','mget'}

for n in [10**6, 5 * 10**6, 30 * 10**6]:
    for c in [10, 100, 500]:
        for p in [0, 3]:
            print ("redis-benchmark -h {ip} -t {tests} -c "
                   "{c} -n {n} -d 5 -P {p}".format(c=c, n=n, p=p, ip=ip, tests="".join(tests)))

