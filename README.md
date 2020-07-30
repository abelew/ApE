# A Plasmid Editor

This repository contains the TCL script for the plasmid editor written by
M. Wayne Davis.

http://jorgensen.biology.utah.edu/wayned/ape/

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

# What did I do?

This repository is an essentially unchanged copy of M. Wayne Davis' code.  If
one wishes to perform a similar extraction on a different version of the editor,
here is what I did.  In an attempt to make that easier, I included a copy of the
tclkit[2] and sdx.kit[3] I used.

1.  Go into the bin/ directory of the repository.
2.  Download the ApE windows zip file and unzip it into the $(pwd).
3.  Extract the data with: `tclkit sdx.kit unwrap ApE_win_current.exe`,
    this creates the ApE_win_current.vfs/ directory containing the tcl
    script, the accessory files, and the miscellaneous stuff used by
    tclkit/sdx.
4.  Copy out the main script with:
    `cp ApE_win_current.vfs/lib/app-AppMain/AppMain.tcl ../..`
5.  Copy the accessory files, I usually delete the existing files first, but I
    doubt that is necessary: `rsync -av ApE_win_current.vfs/Accessory\ Files/ ../../`
6.  Delete the windows specific material: `rm -r ../../lib`
7.  Clean up the $(pwd).

# References

1. https://modules.readthedocs.io/en/latest/
2. https://wiki.tcl-lang.org/page/basekit (I used the original)
3. https://wiki.tcl-lang.org/page/sdx
