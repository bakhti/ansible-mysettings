ansible-mysettings
==================

I wanted to put my dot files into git, but then I thought that I'd like not only dot-files, but also instructions with commands to run. Ansible role would be better for that and I can keep my dot-files there, and even make them into templates. This Ansible role should help to setup my working environment and it's a place to track all the small changes I sometimes do to the system.

Requirements
------------

None for now.

Role Variables
--------------

```yaml
conkeror:
  - path: /home/user/src
  - desktop_dir: /home/user/.local/share/applications
  - icon: /usr/share/icons/gnome/48x48/apps/web-browser.png 
  - exec: conkeror
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
