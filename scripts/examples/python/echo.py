#!/usr/bin/env python2

# Simple XenAPI plugin
from __future__ import absolute_import
from __future__ import division
from __future__ import print_function
from __future__ import unicode_literals
from future import standard_library
standard_library.install_aliases()
from builtins import *
import XenAPIPlugin, time

def main(session, args):
    if "sleep" in args:
        secs = int(args["sleep"])
        time.sleep(secs)
    return "args were: %s" % (repr(args))

if __name__ == "__main__":
    XenAPIPlugin.dispatch({"main": main})


