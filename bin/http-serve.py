#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""launch small http server
"""

import sys

try:
    from SimpleHTTPServer import SimpleHTTPRequestHandler
except ImportError:
    from http.server import SimpleHTTPRequestHandler

try:
    from SocketServer import TCPServer as HTTPServer
except ImportError:
    from http.server import HTTPServer

# simple web server
# serves files relative to the current directory.

server_port = 8080
try:
    server_port = int(sys.argv[1])
except:
    pass

httpd = HTTPServer(("", server_port), SimpleHTTPRequestHandler)
print("serving at port {0}".format(server_port))
httpd.serve_forever()
