
"""
commands = get,set,ping,(mset,mget if finished)
n = 1.000.000-5.000.000-30.000.000
c = 1 - 10 - 100 - 1000 - 10000
r = 100000
pipeline = yes/no
"""

ip = "128.59.21.192"

tests = {'ping','get','set','mset','mget'}
for n in [10**6, 5 * 10**6, 10**7, 5*10**7]:
    for c in [10, 100, 500, 1000]:
        for p in [1, 10, 50]:
            print ("redis-benchmark -h {ip} -t {tests} -c "
                   "{c} -n {n} -d 20 -r {r} -P {p}".format(
                       r=max(n/100,1),
                       c=c,
                       n=n,
                       p=p,
                       ip=ip,
                       tests=",".join(tests)))

