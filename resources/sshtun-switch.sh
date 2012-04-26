#! /bin/bash


# This path and file needs to describe the file that will be read by
# sshtun on the remote system via http

switchDir=/var/www
switchFile=tflag


switchFileDown="${switchFile}-down"
switchFileUp="${switchFile}-up"

function checkSetup {
   # Handle first-time creation of the files
   # Possible problem: permissions for the http server?

   [ -f "$switchFileDown" ] || echo 0 > $switchFileDown
   [ -f "$switchFileUp" ] || echo 1 > $switchFileUp
}


cd $switchDir

case "$1" in
   "up")
      checkSetup
      rm $switchFile
      ln -s $switchFileUp $switchFile
      ;;
   "down")
      checkSetup
      rm $switchFile
      ln -s $switchFileDown $switchFile
      ;;
   *)
      echo "usage: $0 [up|down]"
      ;;
esac
