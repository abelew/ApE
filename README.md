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

I made a couple of changes to this repository in an attempt to make it easier to
install/use on a linux host.  The hosts I work with all use environment
modules[1] and so I am providing an example module file and instructions
assuming an environment similar to mine.

## Environment modules installation

Our environment modules installation is based in /sw where all the modules are
in /sw/modules and the project root is /sw/local.  Thus the top of the module
file defines those directories and attempts to automagically detect everything
else.  With that in mind, installation using environment modules is the
following:

1. Pull the repository into the directory: /sw/local/ape/some_version_number
2. Copy the modulefile (currently modules/202007) to /sw/modules/ape/some_version_number
3. `module add ape`
4. Invoke `ape` and the first time it should ask for the location of the
   data files.

## Manual installation

The installation without environment modules is also quite simple:

1. pull the repository.
2. edit bin/ape so that the variable APE_DIR points to the location of the
   repository.
3. Put bin/ape in an alias or in the PATH.
4. The first time ape is run on a new host, it will bring up a little box
   asking where the various files are located.  Tell it to go to the downloaded
   location.
