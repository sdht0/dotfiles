from http.server import HTTPServer, BaseHTTPRequestHandler
import subprocess
import psutil

def get_stats():
    output = []
    applications = [('firefox',lambda cmdline_str: "['/usr/lib/firefox-developer-edition/firefox']" == cmdline_str),
                 ('intellij',lambda cmdline_str: "bin/idea.sh" in cmdline_str),
                 ('slack',lambda cmdline_str: "['/usr/lib/slack/slack']" == cmdline_str),
                 ('vscode',lambda cmdline_str: "/usr/lib/code/code.js" in cmdline_str)]
    data = {name: list() for name,_ in applications}
    for proc in psutil.process_iter():
        try:
            cmdline_str = str(proc.cmdline())
        except (psutil.NoSuchProcess,psutil.AccessDenied):
            continue

        for name,check_fn in applications:
            if check_fn(cmdline_str):
                data[name].append(proc)
                data[name].extend(proc.children(recursive=True))
                break    # Add a process to only one search

    attribute_name = "application_memory_usage"
    output.append("# HELP %s Memory information for individual tracked applications." % attribute_name)
    output.append("# TYPE %s gauge" % attribute_name)
    for name,_ in applications:
        details = {k: list() for k in ['rss', 'shared', 'pss']}
        print(name)
        for proc in data[name]:
            print(proc)
            mem = proc.memory_full_info()
            for detail in details:
                details[detail].append(getattr(mem,detail))
        for detail,values in details.items():
            output.append('%s{application="%s",memory_type="%s"} %s' % (attribute_name, name, detail, sum(values)/1024/1024/1024))

    attribute_name = "application_process_count"
    output.append("# HELP %s Number of processes for individual tracked applications." % attribute_name)
    output.append("# TYPE %s gauge" % attribute_name)
    for name,_ in applications:
        output.append('%s{application="%s"} %s' % (attribute_name, name, len(data[name])))

    output.append("# HELP node_exporter_build_info A metric with a constant '1' value.")
    output.append("# TYPE node_exporter_build_info gauge")
    output.append('node_exporter_build_info{branch="master"} 1')
    return output

class SimpleHTTPRequestHandler(BaseHTTPRequestHandler):

    def do_GET(self):
        results = get_stats()
        self.send_response(200)
        self.end_headers()
        for result in results:
            self.wfile.write(("%s\n" % result).encode('utf-8'))

httpd = HTTPServer(('localhost', 9101), SimpleHTTPRequestHandler)
httpd.serve_forever()
