#!/bin/sh
DoExitAsm ()
{ echo "An error occurred while assembling $1"; exit 1; }
DoExitLink ()
{ echo "An error occurred while linking $1"; exit 1; }
OFS=$IFS
IFS="
"
/Library/Developer/CommandLineTools/usr/bin/ld     -weak_framework AppKit -weak_framework UserNotifications      -order_file /Users/pedro/Projects/SB_MMO/ekron-realms/Projects/symbol_order.fpc -multiply_defined suppress -L. -o /Users/pedro/Projects/SB_MMO/ekron-realms/Projects/DebuggerMain `cat /Users/pedro/Projects/SB_MMO/ekron-realms/Projects/link51799.res` -filelist /Users/pedro/Projects/SB_MMO/ekron-realms/Projects/linkfiles51799.res
if [ $? != 0 ]; then DoExitLink ; fi
IFS=$OFS
