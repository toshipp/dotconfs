#!/bin/sh
#
# ~/.xinitrc
#
# Executed by startx (run your window manager from here)

if [ -d /etc/X11/xinit/xinitrc.d ]; then
  for f in /etc/X11/xinit/xinitrc.d/*; do
    [ -x "$f" ] && . "$f"
  done
  unset f
fi

# set cursor
xsetroot -cursor_name left_ptr

# set resources (for urxvt)
xrdb ~/.Xresources &

# set key repeat
xset r rate 250 70

# run dropbox if exist
which dropboxd && dropboxd &

# setup im
export GTK_IM_MODULE=ibus
export XMODIFIERS=@im=ibus
export QT_IM_MODULE=ibus
ibus-daemon -drx

# start wm
exec qtile
