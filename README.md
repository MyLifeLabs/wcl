wcl - counting lines with progress indicator or "wc -l" for the impatient
=========================================================================

`wcl` is a replacement for `wc -l` useful for interactive use
with large data files. Like `wc -l` it counts the number of lines
in files and displays the total. Unlike `wc -l`, wcl displays a progress 
meter and an estimate of the total number of lines as it progresses.

While in progress, it looks like this:

```
$ wcl aaaaaaaaaaaaa.dat bbbbbbbbbbbb.dat ccccccccc.dat
31,261,832 aaaaaaaaaaaaa.dat
 11% [bbbbbbbbbbbb.dat] projected line count: 443,486,924
```

When complete, it looks like this:

```
$ wcl aaaaaaaaaaaaa.dat bbbbbbbbbbbb.dat ccccccccc.dat
  31,261,832 aaaaaaaaaaaaa.dat
 167,622,458 bbbbbbbbbbbb.dat
 244,602,594 ccccccccc.dat
 443,486,884 total
```

Installation
------------

Compiling the executable requires a standard installation of OCaml.

```
$ make
$ make install  # Installation directory defaults to $HOME/bin.
```

`PREFIX` and `BINDIR` are supported, so if you want to install `wcl`
in `/usr/local`,
just do:

```
$ sudo make PREFIX=/usr/local install
```

Uninstallation:

```
$ make uninstall
```

Known limitations
-----------------

* progress bar probably doesn't show up properly on Windows
  (please let me know; I don't have access to a Windows machine)
