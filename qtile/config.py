from libqtile.config import Key, Screen, Group
from libqtile.command import lazy
from libqtile import layout, bar, widget, hook
#from mylayout import Wmfs

sup = "mod4"

keys = [
    # Switch between windows in current stack pane
    Key(
        [sup], "k",
        lazy.layout.down()
    ),
    Key(
        [sup], "j",
        lazy.layout.up()
    ),

    # Move windows up or down in current stack
    Key(
        [sup, "control"], "k",
        lazy.layout.shuffle_down()
    ),
    Key(
        [sup, "control"], "j",
        lazy.layout.shuffle_up()
    ),
    Key(
        [sup], "bracketleft",
        lazy.layout.grow()
    ),
    Key(
        [sup], "bracketright",
        lazy.layout.shrink()
    ),
    # Switch window focus to other pane(s) of stack Key(
    Key(
        [sup], "space",
        lazy.layout.next()
    ),

    Key([sup], "Return", lazy.spawn("urxvt")),
    Key([sup], "m", lazy.spawn("nautilus --no-desktop")),
    Key([sup], "e", lazy.spawn("emacs")),
    Key([sup], "c", lazy.spawn("google-chrome")),
    Key([sup], "p", lazy.spawn("vlc")),
    Key([sup], "l", lazy.spawn("slimlock")),

    # Toggle between different layouts as defined below
    Key([sup], "Tab",    lazy.nextlayout()),
    Key([sup], "w",      lazy.window.kill()),

    Key([sup], "r", lazy.spawncmd(prompt="$")),

    Key([sup, "control"], "r", lazy.restart()),
    Key([sup, "control"], "q", lazy.shutdown()),
]

groups = [
    Group("1"),
    Group("2"),
    Group("3"),
    Group("4"),
]
for i in groups:
    # mod1 + letter of group = switch to group
    keys.append(
        Key([sup], i.name, lazy.group[i.name].toscreen())
    )

    # mod1 + shift + letter of group = switch to & move focused window to group
    keys.append(
        Key([sup, "shift"], i.name, lazy.window.togroup(i.name))
    )

layouts = [
    layout.MonadTall(),
    layout.RatioTile(border_focus='#ff0000', border_width=2),
    layout.TreeTab()
    ]

screens = [
    Screen(
        bottom = bar.Bar(
                    [
                        widget.GroupBox(inactive='808080',fontsize=16),
                        widget.WindowName(),
                        widget.Prompt(),
                        widget.CurrentLayout(),
                        widget.Systray(),
                        widget.Clock('%Y-%m-%d %a %I:%M %p'),
                    ],
                    30,
                ),
    ),
]

follow_mouse_focus = True
cursor_warp = False
floating_layout = layout.Floating()
mouse = ()

@hook.subscribe.client_new
def float_dialog(window):
    if(window.window.get_wm_type() == 'dialog'
       or window.window.get_wm_transient_for()):
        window.floating = True
