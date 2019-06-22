from http.server import HTTPServer, BaseHTTPRequestHandler
import subprocess
import psutil

def get_stats():
    output = []
    process_strings = ['firefox','intellij','slack', 'electron/electron']
    data = {k: list() for k in process_strings}
    for proc in psutil.process_iter():

        try:
            cmdline = str(proc.cmdline())
        except (NoSuchProcess,AccessDenied):
            continue

        for process_string in process_strings:
            if process_string in cmdline:
                data[process_string].append(proc)

    attribute_name = "process_memory_usage"
    output.append("# HELP %s Memory information for individual tracked processes." % attribute_name)
    output.append("# TYPE %s gauge" % attribute_name)
    for process_string in process_strings:
        details = {k: list() for k in ['rss', 'shared', 'pss']}
        for proc in data[process_string]:
            mem = proc.memory_full_info()
            for detail in details:
                details[detail].append(getattr(mem,detail))
        for detail,values in details.items():
            output.append('%s{process="%s",memory_type="%s"} %s' % (attribute_name, process_string, detail, sum(values)/1024/1024/1024))
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
