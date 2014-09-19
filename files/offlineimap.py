#!/usr/bin/env python
import re
import os


def get_authinfo_password(machine, login, port):
    s = "machine {0} login {1} port {2} password ([^ ]*)".format(machine, login, port)
    p = re.compile(s)
    authinfo = os.popen("gpg -q --no-tty -d ~/.authinfo.gpg").read()
    return p.search(authinfo).group(1)
