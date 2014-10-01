#!/usr/bin/env python
# -*- coding: utf-8 -*-

from libqtile.command import lazy
from libqtile import layout, bar, widget, hook
try:
    from libqtile.manager import Key, Group
except ImportError:
    from libqtile.config import Key, Group

from libqtile.manager import Click, Drag, Screen

screens = [Screen(top = bar.Bar(
    [
        widget.GroupBox(margin_y = 1,
                        margin_x = 1,
                        borderwidth = 1,
                        padding = 1,),
        widget.Prompt(),
        widget.Sep(),
        widget.Volume(),
        widget.Battery(energy_now_file = "charge_now",
                       energy_full_file = "charge_full",
                       power_now_file = "current_now",
                       update_delay = 5,
                       foreground = "7070ff",
                       charge_char = u'↑',
                       discharge_char = u'↓',),
        widget.CurrentLayout(),
        widget.Sep(),
        widget.Systray(),
        widget.Clock(foreground = "a0a0a0",
                     fmt = "%a, %d %b %H:%M",),
    ], 24,),),
]

# Commands to spawn
class Commands(object):
    browser = 'conkeror'
    dmenu = 'dmenu_run -i -b -p ">>>" -fn "-*-fixed-*-*-*-*-18-*-*-*-*-*-*-*" -nb "#15181a" -nf "#fff" -sb "#333" -sf "#fff"'
    file_manager = 'nautilus --no-desktop'
    lock_screen = 'mate-screensaver-command -l'
    screenshot = 'mate-screenshot'
    terminal = 'POWERLINE_CONFIG_COMMAND=${HOME}/.local/bin/powerline-config stterm -f "Source Code Pro for Powerline:pixelsize=15" -e tmux'
    trackpad_toggle = 'xinput set-int-prop "SynPS/2 Synaptics TouchPad" "Device Enabled" 8 $(xinput list-props "SynPS/2 Synaptics TouchPad" |grep -c "Device Enabled.*0$")'
    volume_up = 'amixer -q -c 1 sset Master 5dB+'
    volume_down = 'amixer -q -c 1 sset Master 5dB-'
    volume_toggle = 'amixer -q -D pulse sset Master 1+ toggle'
    light_up =  'xbacklight -inc 30'
    light_down =  'xbacklight -dec 30'
    head_toggle = 'xrandr --output HDMI1 --right-of LVDS1 --auto'

mod = 'mod4'
alt = 'mod1'
keys = [
    Key([mod, 'control'], 'q', lazy.shutdown()),
    Key([mod, 'control'], 'r', lazy.restart()),
    Key([mod, 'control'], 'l', lazy.spawn(Commands.lock_screen)),

    Key([mod], 'w', lazy.window.kill()),
    Key([mod], 'f', lazy.window.toggle_floating()),
    Key([mod], 'F12', lazy.window.toggle_fullscreen()),

    Key([mod], 'k',               lazy.layout.down()),
    Key([mod], 'j',               lazy.layout.up()),
    Key([mod], 'h',               lazy.layout.previous()),
    Key([mod], 'l',               lazy.layout.next()),
    Key([mod], 'Left',            lazy.to_screen(0)),
    Key([mod], 'Right',           lazy.to_screen(1)),
    Key([mod, 'shift'], 'space',  lazy.layout.rotate()),
    Key([mod, 'shift'], 'Return', lazy.layout.toggle_split()),
    Key([alt], 'Tab',             lazy.nextlayout()),

    # interact with prompts
    Key([mod], "r",              lazy.spawncmd()),
    Key([mod], "g",              lazy.switchgroup()),

    # Commands: Application Launchers
    Key([mod], 'space', lazy.spawn(Commands.dmenu)),
    Key([mod], 'n', lazy.spawn(Commands.browser)),
    Key([mod], 'e', lazy.spawn(Commands.file_manager)),
    Key([mod], 'Return', lazy.spawn(Commands.terminal)),

    # Commands: Volume Controls
    Key([], 'XF86AudioRaiseVolume', lazy.spawn(Commands.volume_up)),
    Key([], 'XF86AudioLowerVolume', lazy.spawn(Commands.volume_down)),
    Key([], 'XF86AudioMute', lazy.spawn(Commands.volume_toggle)),

    Key([], 'XF86TouchpadToggle', lazy.spawn(Commands.trackpad_toggle)),
    Key([], 'XF86MonBrightnessUp', lazy.spawn(Commands.light_up)),
    Key([], 'XF86MonBrightnessDown', lazy.spawn(Commands.light_down)),
    # TODO: make the following key switchable - LVDS1 only / additional head
    Key([], 'XF86Display', lazy.spawn(Commands.head_toggle)),
]

# This allows you to drag windows around with the mouse if you want.
mouse = [
    Drag([mod], "Button1", lazy.window.set_position_floating(),
        start=lazy.window.get_position()),
    Drag([mod], "Button3", lazy.window.set_size_floating(),
        start=lazy.window.get_size()),
    Click([mod], "Button2", lazy.window.bring_to_front())
]

group_names = [
    ("emacs", {'layout': 'max'}),
    ("web", {'layout': 'max'}),
    ("mail", {'layout': 'max'}),
    ("chat", {'layout': 'tile'}),
    ("etc", {})
]

groups = [Group(name, **kwargs) for name, kwargs in group_names]

for index, (grp, kwargs) in enumerate(group_names, 1):
    keys.extend([
        Key([mod], str(index), lazy.group[grp].toscreen()),
        Key([mod, "shift"], str(index), lazy.window.togroup(grp)),
        Key([mod, "control"], str(index), lazy.group.swap_groups(grp))
    ])

layouts = [
    layout.Max(),
    layout.Stack(stacks=2, border_width=1),
    layout.Tile(ratio=0.25),
]

float_windows = set([
    "nagstamon.py"
    "feh",
    "x11-ssh-askpass",
    "pinentry"
])

# wm_window_role: PasswordManager, Preferences
def should_be_floating(w):
    wm_class = w.get_wm_class()
    if isinstance(wm_class, tuple):
        for cls in wm_class:
            if cls.lower() in float_windows:
                return True
    else:
        if wm_class.lower() in float_windows:
            return True
    return w.get_wm_type() == 'dialog' or bool(w.get_wm_transient_for())

@hook.subscribe.client_new
def dialogs(window):
    if should_be_floating(window.window):
        window.floating = True
