#!/usr/bin/env python2

from __future__ import print_function
from __future__ import absolute_import
from __future__ import division
from __future__ import unicode_literals
from future import standard_library
standard_library.install_aliases()
from builtins import *
import xmlrpc.client
server = xmlrpc.client.Server("http://melton:8086");
session = server.session.login_with_password("root", "xenroot", "1.0", "xen-api-list-vms.py")['Value']
print(session)
vms = server.VM.get_all(session)['Value']
print(vms)
#for vm in vms:
#    print vm,server.VM.get_kernel__kernel(session, vm)
