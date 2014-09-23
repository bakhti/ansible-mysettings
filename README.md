ansible-mysettings
==================

I wanted to put my dot files into git, but then I thought that I'd like not only dot-files, but also instructions with commands to run. Ansible role would be better for that and I can keep my dot-files there, and even make them into templates. This Ansible role should help to setup my working environment and it's a place to track all the small changes I sometimes do to the system.

Requirements
------------

None for now.

Role Variables
--------------

```yaml
---
myfullname: John Smith
mymailfolder: Mail
myapps:
  - name: qtile
    cfgpath: .config/qtile
    files:
      - config.py
  - name: emacs
    cfgpath: .emacs.d
    files:
      - init.el
      - custom.el
      - settings.el
      - packages.el
  - name: conkeror
    cfgpath: .conkerorrc
    files:
      - init.js
      - webjumps.js
      - functions.js
myemail:
  - name: account1
    type: IMAP
    ssl: yes
    sslcacertfile: /etc/ssl/certs/ca-certificates.crt
    remotehost: imap.company.org
    remoteuser: my@company.org
	primary: yes
```

Dependencies
------------

None for now.

Example Playbook
----------------

    - hosts: localhost
      roles:
         - ansible-mysettings

License
-------

[![CC-BY-SA](http://i.creativecommons.org/l/by-sa/3.0/80x15.png)](http://creativecommons.org/licenses/by-sa/4.0/)

Author Information
------------------

http://bakhti.github.io
