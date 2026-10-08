"""Serve only the generated test root on loopback; never the production tree."""
from pathlib import Path
from http.server import ThreadingHTTPServer, SimpleHTTPRequestHandler
from functools import partial
import threading
import webbrowser
import urllib.request
import socket
import os
import sys

repo = Path(__file__).resolve().parents[2]
base = repo / '.rsssf_test_site'
pointer = base / 'current.txt'
if not pointer.exists():
    raise SystemExit('Run scripts/EuropeanFootball/RSSSF_build_test_site.R first.')
root = Path(pointer.read_text(encoding='utf-8-sig').strip()).resolve()
if not root.is_relative_to(base.resolve()) or not (root / 'README.txt').is_file():
    raise SystemExit('Invalid or incomplete test build.')
class ExclusiveTestServer(ThreadingHTTPServer):
    # On Windows, http.server's SO_REUSEADDR can let another server bind the
    # same port. Requests may then reach the OLD build. Demand exclusive use.
    allow_reuse_address = False

    def server_bind(self):
        if os.name == 'nt':
            self.socket.setsockopt(socket.SOL_SOCKET, socket.SO_EXCLUSIVEADDRUSE, 1)
        super().server_bind()

class TestHandler(SimpleHTTPRequestHandler):
    def do_GET(self):
        if self.path == '/__rsssf_build':
            payload = str(root).encode('utf-8')
            self.send_response(200)
            self.send_header('Content-Type', 'text/plain; charset=utf-8')
            self.send_header('Content-Length', str(len(payload)))
            self.end_headers()
            self.wfile.write(payload)
        else:
            super().do_GET()

    def end_headers(self):
        self.send_header('Cache-Control', 'no-store')
        super().end_headers()

# A previous build may still be running. Reuse ONLY an exact build match;
# otherwise start the latest build on another loopback port without stopping it.
for port in range(8001, 8011):
    url = f'http://127.0.0.1:{port}/'
    try:
        server = ExclusiveTestServer(('127.0.0.1', port), partial(TestHandler, directory=str(root)))
        break
    except OSError:
        try:
            with urllib.request.urlopen(url + '__rsssf_build', timeout=1) as response:
                active = response.read(4096).decode('utf-8')
            if active == str(root):
                print(f'Opening existing CURRENT build: {url}\n{root}', flush=True)
                if '--no-browser' not in sys.argv:
                    webbrowser.open(url)
                raise SystemExit(0)
        except (OSError, UnicodeError):
            pass
else:
    raise SystemExit('Ports 8001-8010 are occupied. Close an old test-server window and try again.')
print(f'RSSSF TEST ONLY: {url}\nBuild: {root}', flush=True)
if '--no-browser' not in sys.argv:
    threading.Timer(0.5, lambda: webbrowser.open(url)).start()
try:
    server.serve_forever()
except KeyboardInterrupt:
    pass
finally:
    server.server_close()
