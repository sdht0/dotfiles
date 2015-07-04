#!/bin/sh
#
# Sync users' files to their $HOMEs
# Based on http://semicomplete.googlecode.com/svn/sysadvent/2008/day11/synchome.sh
#
# Usage: Run as root. Put it in a crontab if required.
#
# * Wed Apr 28 Naresh V. <naresh.ve@directi.com> - 0.1-1
# - Initial release
#
# * Tue May 03 Naresh V. <naresh.ve@directi.com> - 0.1-2
# - Fix .ssh permissions
#
# * Fri Aug 05 Naresh V. <naresh.ve@directi.com> - 0.1-3
# - Copy /etc/skel on first-time $HOME creation

PROG="$(basename $0)"

SOURCE="ops-git.media.net::user-files"

# check if rsync point is live
rsync -q $SOURCE
if [ $? -ne 0 ]; then
    echo "$PROG: rsync fatal error" >&2
    exit 1
fi

# since the rsync mount point itself is a git clone
# ignore ., .git, .gitignore, etc.
# i.e. only consider non-dot dirs
#rsync $SOURCE | awk '/^d/ && $NF !~ /^\./ { print $NF }'
if [ -n "$1" ];then
    user=$1
else
    user='siddhartha.s'
fi
getent passwd $user > /dev/null
if [ $? -ne 0 ]; then
    echo "[ SKIP ] $user doesn't exist" >&1
    continue
fi

rsync $SOURCE/$user > /dev/null 2>&1
if [ $? -ne 0 ]; then
    echo "[ SKIP ] $user doesn't exist in git repo" >&1
    continue
fi

userhome="$(getent passwd $user | cut -f 6 -d : )"

if [ ! -d "$userhome" ]; then
    gid="$(id $user | sed -re 's/^.* gid=([0-9]+)[^0-9].*$/\1/')"
    # create $USERHOME if it doesn't exist
    install -d -o $user -g $gid -m 0750 $userhome
    # and copy of skel
    rsync -r /etc/skel/ $userhome
    chown -R $user $userhome
    # all of this should be one-time
fi

if [ -d "$userhome" ]; then
    group="$(id $user | sed 's/^.* gid=.*(\(.*\)).*/\1/')"
    # rsync user dir
    rsync -vr --exclude=.git $SOURCE/$user/ $userhome/|while read i ;do if [ -e "$userhome/$i" ];then chown $user:$group  "$userhome/$i"; fi ;done
    echo "[ SYNC ] $user exists" >&1
    # fix permissions on .ssh
    chmod 0700 $userhome/.ssh
    chmod 0600 $userhome/.ssh/authorized_keys
    chown -R $user:$group $userhome/.ssh
    chown $user:$group $userhome/.bash*
    if [ -f $userhome/.ssh/id_rsa ]
    then
        chmod 600 $userhome/.ssh/id_rsa
    fi
    # Fix ownership
        #chown -R $user:$group $userhome/
    uname -r | grep -q lve
    if [ $? -eq 0 ]
    then
        rpm -q cagefs > /dev/null 2>&1
        if [ $? -eq 0 ]
        then
            if [ $(/usr/sbin/cagefsctl --cagefs-status) == "Enabled" ]
            then
                if [ $(/usr/sbin/cagefsctl --user-status ${user}) == "Enabled" ]
                then
                    /usr/sbin/cagefsctl --disable ${user}
                fi
            fi
        fi
    fi
fi

