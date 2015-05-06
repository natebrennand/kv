from sys import argv
import re


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
    return (title, num, length, clients, size, keep_alive, throughput, tiers)


def gen_cdf(title, num, length, clients, size, keep_alive, throughput, tiers):
    filename = "{title}_w_{clients}".format(title=title, clients=clients)
    with open(filename, 'wb') as f:
        for (percent, latency) in tiers:
            f.write("({percent}, {latency})\n".format(percent=percent, latency=latency))


def gen_graph(res_array):
    pass


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
        gen_cdf(*r)
