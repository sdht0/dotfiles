export KF5=/opt/kde5
export QTDIR=/usr/lib/qt

export PATH=$KF5/bin:$QTDIR/bin:$PATH

export QT_PLUGIN_PATH=$KF5/lib/qt5/plugins:$QTDIR/plugins:$QT_PLUGIN_PATH
export QML2_IMPORT_PATH=$KF5/lib/qml:$QTDIR/qml
export QML_IMPORT_PATH=$QML2_IMPORT_PATH

export XDG_DATA_DIRS=$KF5/share:/usr/share
export XDG_CONFIG_DIRS=$KF5/etc/xdg:/etc/xdg

export LD_LIBRARY_PATH=/opt/kde5/lib/:$LD_LIBRARY_PATH

export XDG_DATA_HOME=/home/sdh/.kde5/local
export XDG_CONFIG_HOME=/home/sdh/.kde5/config
export XDG_CACHE_HOME=/home/sdh/.cache

