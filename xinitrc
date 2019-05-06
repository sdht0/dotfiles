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

export KF5=/opt/kde
export QTDIR=/usr/lib/qt

export PATH=$KF5/bin:$PATH

export QT_PLUGIN_PATH=$KF5/lib/plugins:$QTDIR/plugins:$QT_PLUGIN_PATH
export QML2_IMPORT_PATH=$KF5/lib/qml:$QTDIR/qml
export QML_IMPORT_PATH=$QML2_IMPORT_PATH

export XDG_DATA_DIRS=$KF5/share:/usr/share
export XDG_CONFIG_DIRS=$KF5/etc/xdg:/etc/xdg

#export LD_LIBRARY_PATH="$KF5/lib:$LD_LIBRARY_PATH"

#export XDG_DATA_HOME=/home/sdh/.kde5/local
#export XDG_CONFIG_HOME=/home/sdh/.kde5/config
#export XDG_CACHE_HOME=/home/sdh/.cache

# Xephyr -ac -br -noreset -screen 1280x720 :1 &
# DISPLAY=:1
#
# startx

exec startkde
