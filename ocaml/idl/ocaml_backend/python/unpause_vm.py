#!/usr/bin/env python2

from __future__ import absolute_import
from __future__ import division
from __future__ import print_function
from __future__ import unicode_literals
from future import standard_library
standard_library.install_aliases()
from builtins import *
import xmlrpc.client
server = xmlrpc.client.Server("http://localhost:8086");
session = server.Session.do_login_with_password("user", "passwd", "1.0", "xen-api-unpause-vm.py")['Value']
server.VM.do_unpause(session, '7366a41a-e50e-b891-fa0c-ca5b4d2e3f1c')
