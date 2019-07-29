# A Plasmid Editor

This repository contains the TCL script for the plasmid editor written by
M. Wayne Davis.

http://jorgensen.biology.utah.edu/wayned/ape/

It was extracted by doing the following:

1. Create a directory for this repository.
2. Download the current version of the windows executable.
3. Invoke tclkit sdx.kit unwrap ApE_win_current.exe
4. Copy the ApE_win_current.vfs/lib/app-AppMain/AppMain.tcl to ape.tcl in the
   repository.
5. rsync the 'ApE_win_current.vfs/Accessory Files/' directory into the repository.
6. Delete the windows-specific 'lib/' directory.

If one wishes to install this on a linux host:

1. pull the repository.
2. edit ape.sh so that the variable ape_dir points to the location of the
   repository.
3. Put ape.sh in an alias or in the PATH.
4. The first time ape.sh is run on a new host, it will bring up a little box
   asking where the various files are located.  Tell it to go to the downloaded
   location.
