#!/usr/bin/expect -f
set config [lindex $argv 0]
set username [lindex $argv 1]
set password [lindex $argv 2]
set response [lindex $argv 3]
spawn sudo openvpn --config "$config"
#match_max 100000
expect "Username:"
send -- "$username\r"
log_user 0
expect "Password:"
send -- "$password\r"
expect "Response:"
send -- "$response\r"
log_user 1
interact
