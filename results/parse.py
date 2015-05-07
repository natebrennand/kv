from sys import argv
from itertools import groupby
import re


class Bench(object):
    def __init__(self, title, num, length, clients, size, keep_alive, throughput, tiers):
        self.title = title
        self.num = num
        self.length = length
        self.clients = int(clients)
        self.size = size
        self.keep_alive = keep_alive
        self.throughput = throughput
        self.tiers = tiers


def process_benchmark(f):
    titleMatch = re.match(r"====== (?P<title>.*) ======", f.readline())
    if not titleMatch:
        return None
    title = titleMatch.group("title").replace(" ", "_").replace("(", "_").replace(")", "_")
    perf = re.match(r"  (?P<num>\d+) requests completed in (?P<time>.*) seconds", f.readline())
    num, length = perf.group("num"), perf.group("time")
    clients = re.match(r"  (?P<clients>\d+) parallel clients", f.readline()).group("clients")
    size = re.match(r"  (?P<size>\d+) bytes payload", f.readline()).group("size")
    keep_alive = bool(re.match(r"  keep alive: (?P<keep_alive>\d)", f.readline()).group("keep_alive"))
    f.readline()

    text = ""
    tiers = []
    while True:
        text = f.readline()
        data = re.match(r"(?P<percent>\d+\.\d+)% <= (?P<latency>[\d]+) milliseconds", text)
        if not data:
            break
        tiers.append((data.group("percent"), data.group("latency")))


    throughput = re.match(r"(?P<throughput>\d+\.\d+) requests per second", text).group("throughput")
    f.readline()
    return Bench(title, num, length, clients, size, keep_alive, throughput, tiers)


def gen_cdf(b):
    filename = "{title}_w_{clients}.cdf.results".format(title=b.title, clients=b.clients).replace("__", "_", 10)
    with open(filename, 'wb') as f:
        for (p, l) in b.tiers:
            f.write("({percent}, {latency})\n".format(percent=p, latency=l))


def gen_graph(res_array):
    commands = [r"^GET", r"^SET", r"^MSET", r"^PING_BULK_"]
    pipelines = [r"pipeline_1_$", r"pipeline_10_$", r"pipeline_50_$"]

    for filename in set(map(lambda a: a.title, res_array)):
        with open(filename+".results", 'w') as f:
            f.write("")


    for c in commands:
        results = filter(lambda r: re.match(c, r.title), res_array)

        for p in pipelines:
            results2 = filter(lambda r: re.search(p, r.title), results)

            client_func = lambda r: r.clients
            results2 = sorted(results2, key=client_func)
            for c_num, nums in groupby(results2, key=client_func):
                res = [r for r in nums]
                results3 = max(res, key=lambda r: r.throughput)
                print results3.title, c_num, results3.clients, results3.throughput, results3.num

                with open(r.title+".results", 'a') as f:
                    f.write("({}, {})\n".format(c_num, results3.throughput))



if __name__ == '__main__':
    if len(argv) != 2:
        print 'Please provide a filename of benchmark results'
        exit(1)
    filename = argv[1]

    results = []
    with open(filename) as f:
        while f:
            res = process_benchmark(f)
            if not res:
                break
            results.append(res)

    for r in results:
       gen_cdf(r)

    gen_graph(results)

