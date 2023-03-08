#!/usr/bin/env python2

from __future__ import print_function
from __future__ import absolute_import
from __future__ import division
from __future__ import unicode_literals
from future import standard_library
standard_library.install_aliases()
from builtins import *
import xmlrpc.client, sys

# Don't forget to include the port in the url (eg http://melton:8086/)
if len(sys.argv) != 4: 
    raise "Expected arguments: <url> <username> <password>"

server = xmlrpc.client.Server(sys.argv[1]);
session = server.session.login_with_password(sys.argv[2], sys.argv[3], "1.0", "xen-api-event-listen.py")['Value']

server.event.register(session, ["*"])
while True:
    events = server.event.next(session)['Value']
    for event in events:
        print(event['id'], " ", event['class'], " ", event['operation'], " ",event['ref'], " ", end=' ')
	if "snapshot" in list(event.keys()):
	   print("OK")
	else:
	   print("(no snapshot)")
