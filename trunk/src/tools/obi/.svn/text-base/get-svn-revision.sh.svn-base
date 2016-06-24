#!/bin/bash
# get-svn-revision - Determine Subversion repository revision number.

# This tries to work both for "real" Subversion repositories, and for
# SVK local copies of Subversion repositories (the kind of copy that
# can be used for offline work).

# If an .svn directory exists, this is a Subversion repository.
# Otherwise, assume that this is a local SVK copy of a Subversion repository.
# If the revision cannot be determined, return no output.

# Who-to-blame:
# Paul DuBois, paul@mysql.com
# 2005-11-17

# 2006-04-25
# - Enable script to get remote (parent) repository revision, too
#   (use -R option).  Default behavior is to get local revision, as
#   before (use no option or -L option).

function get_local_revision {
  if [ -d .svn ]; then
    #echo "This is a Subversion repository."
    svn info | grep '^Revision' | sed -e 's/Revision: *//'
  else
    #echo "This is a local SVK copy of a Subversion repository."
    # The first 'Copied From:' line presumably is the parent in the
    # SVK depot of this local copy.
    DEPOT=`svk info \
      | grep '^Copied From:' \
      | head -n 1 \
      | sed -e 's/^Copied From: *//' -e 's/,.*//'`
    # Try info for parent if there is no "Copied From: in current directory
    if [ "$DEPOT" = "" ]; then
      DEPOT=`svk info .. \
        | grep '^Copied From:' \
        | head -n 1 \
        | sed -e 's/^Copied From: *//' -e 's/,.*//'`
    fi
    # The info for the depot includes a 'Mirrored From:' line that
    # indicates parent Subversion repository and the revision last
    # mirrored.
    # (If DEPOT is empty, then we are unable to tell the revision
    # and there is no output.)
    if [ "$DEPOT" != "" ]; then
      svk info /$DEPOT | grep '^Mirrored From:' | sed -e 's/.*, Rev\. *//'
    fi
  fi
}

function get_remote_revision {
  if [ -d .svn ]; then
    #echo "This is a Subversion repository."
    URL=`svn info | grep '^URL: ' | sed -e 's/^URL: *//'`
    svn info $URL | grep '^Revision' | sed -e 's/Revision: *//'
  else
    #echo "This is a local SVK copy of a Subversion repository."
    # The first 'Copied From:' line presumably is the parent in the
    # SVK depot of this local copy.
    DEPOT=`svk info \
      | grep '^Copied From:' \
      | head -n 1 \
      | sed -e 's/^Copied From: *//' -e 's/,.*//'`
    # Try info for parent if there is no "Copied From: in current directory
    if [ "$DEPOT" = "" ]; then
      DEPOT=`svk info .. \
        | grep '^Copied From:' \
        | head -n 1 \
        | sed -e 's/^Copied From: *//' -e 's/,.*//'`
    fi
    # The info for the depot includes a 'Mirrored From:' line that
    # indicates parent Subversion repository and the revision last
    # mirrored.
    # (If DEPOT is empty, then we are unable to tell the revision
    # and there is no output.)
    if [ "$DEPOT" != "" ]; then
      URL=`svk info /$DEPOT | grep '^Mirrored From:' \
        | sed -e 's/^Mirrored From: *//' \
        | sed -e 's/,.*//'`
      svn info $URL | grep '^Revision' | sed -e 's/Revision: *//'
    fi
  fi
}

if [ $# -eq 0 -o "$1" = "-L" ]; then
  get_local_revision
elif [ "$1" = "-R" ]; then
  get_remote_revision
else
  echo "Usage: $0 [-L|-R]" 1>&2
fi

