#!/usr/bin/env python2
# Populate a directory of symlinks partitioning VMs by SR
# (c) Anil Madhavapeddy, Citrix Systems Inc, 2008

from __future__ import print_function
from __future__ import absolute_import
from __future__ import division
from __future__ import unicode_literals
from future import standard_library
standard_library.install_aliases()
from builtins import str
from builtins import *
import atexit
import XenAPI
import os, sys
import getopt

def logout():
    try:
        session.xenapi.session.logout()
    except:
        pass
atexit.register(logout)

def usage():
    print("%s [-d <directory>]" % sys.argv[0], file=sys.stderr)
    sys.exit(1)
   
def main(argv):
    session = XenAPI.xapi_local()
    session.xenapi.login_with_password("", "", "1.0", "xen-api-scripts-linkvmsbysr.py")

    try:
        opts, args = getopt.getopt(sys.argv[1:], "hd:", [])
    except getopt.GetoptError as err:
        print(str(err))
        usage()

    dir = None
    for o,a in opts:
        if o == "-d":
            dir = a

    if dir == None:
        usage()
 
    vms = session.xenapi.VM.get_all_records()
    vbds = session.xenapi.VBD.get_all_records()
    vdis = session.xenapi.VDI.get_all_records()
    srs = session.xenapi.SR.get_all_records()

    vms_in_sr = {}

    for vm in vms:
        vmrec = vms[vm]
        # Ignore built-in templates
        if 'default_template' in vmrec['other_config']:
            if vmrec['other_config']['default_template'] == 'true':
                continue
        # Ignore dom0
        if vmrec['is_control_domain']:
            continue
        # Ignore snapshots
        if vmrec['is_a_snapshot']:
            continue

        # for each VM, figure out the set of SRs it uses
        for vbd in vmrec['VBDs']:
            if vbd not in vbds:
                continue
            vdi = vbds[vbd]['VDI']

            # Ignore VBDs with no VDI such as an empty CD VBD
            if vdi == '':
                continue

            if vdi not in vdis:
                continue

            sr = vdis[vdi]['SR']
            if sr not in srs:
                continue

            sruuid = srs[sr]['uuid']
            vmuuid = vmrec['uuid']

            if sruuid not in vms_in_sr:
                vms_in_sr[sruuid] = {}
            vms_in_sr[sruuid][vmuuid] = 1
    
    for sruuid in list(vms_in_sr.keys()):
        linkdir = "%s/by-sr/%s" % (dir, sruuid)
        if os.path.isdir(linkdir):
            print("Directory %s already exists, skipping" % linkdir, file=sys.stderr)
            continue

        try:
            os.makedirs(linkdir)
        except:
            print("Failed to create directory: %s" % linkdir, file=sys.stderr)
        for vmuuid in list(vms_in_sr[sruuid].keys()):
            try:
                src = "../../all/%s.vmmeta" % vmuuid
                targ = "%s/%s.vmmeta" % (linkdir, vmuuid)
                os.symlink(src, targ)
            except:
                print("Failed to create symlink: %s -> %s" % (src, targ), file=sys.stderr)

    session.xenapi.logout()

if __name__ == "__main__":
    main(sys.argv[1:])


