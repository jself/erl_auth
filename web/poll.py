#!/usr/bin/python 

import time
import os
import copy

dirs = {'src':0,
        'priv':0
       }
while 1:
    new = {}
    changed = False
    for d in dirs:
        new[d] = os.stat(d).st_ctime
        if new[d] != dirs[d]:
            changed = True
    if changed:
        os.system("make")
        changed = False
        dirs = copy.copy(new)
    time.sleep(1)


