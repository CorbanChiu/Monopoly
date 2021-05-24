# Installation Instructions

First, download the zip file, and extract it to the directory of your choosing (`unzip final.zip`, `rm final.zip`). Then proceed with the installation directions depending on your OS.


## Windows
To install the graphics package, utilize the apt package manager by entering the command `sudo apt install pkg-config`. This is a dependency for the graphics package. Next, run `opam install graphics`, which utilizes the ocaml package manager to install the graphics package. Upon successful installation, we must next setup XMing.

Install [XMing] (http://www.straightrunning.com/XmingNotes/) and proceed with normal installation. Start XMing after installation such that it is running in the background.
**Note:** for some Windows systems you may need to open Xming through the UI called “XLaunch” (XLaunch is installed with XMing) When running XLaunch, on the third window, check “no Access Control”. All other settings are default.

Finally, run the command (in ubuntu shell) `export DISPLAY=:` which should synchronize the display from Ubuntu to be displayed with XMing.
****Note:** for some Windows systems, instead of the export command above, run the following command: `export DISPLAY="$(grep nameserver /etc/resolv.conf | sed 's/nameserver //'):0"`

## Mac OS

To install the graphics package, move into the desired directory and then enter `sudo port install pkgconfig` into terminal followed by `opam install graphics`

In order to visualize the GUI, it is necessary to install [xQuartz] (https://www.xquartz.org/).

# Make
After successful installation and configuration of your displays (again, make sure xQuartz/XMing are open to visualize the GUI), you can now run the functions described in `Makefile`: `make play`, which initializes the GUI and provides the user with the interface to play the game, `make build`, which builds the system, `make clean`, which cleans up build files, `make test`, which runs our test suite, `make docs`, which builds all of the relevant public doucmentation, and`make check`, which ensures that the OCaml files and functions are configured properly.
