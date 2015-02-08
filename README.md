# sshtun


## Synopsis

Wrapper daemon to manage an ssh tunnel (Haskell)


## Description

This is a daemon that executes an ssh command to form a secure
tunnel and then blocks on it. If the tunnel goes down, sshtun will
attempt to reestablish it. It can also be set up to monitor a file
on an http server to determine if the tunnel should be up or not,
so you can switch it on or off remotely.

It's this last behavior, the switching on and off remotely,
that makes this project differ from the [simpler sshtun shell
script](http://hub.darcs.net/dino/scripts/browse/sshtun) I wrote. The switching
behavior together with blocking on the tunnel requires separate
threads and so I attacked the problem with Haskell.

For usage information and more details, see the project's [README file](http://hub.darcs.net/dino/sshtun/browse/README.md)


## Getting source

- Download the cabalized source package [from Hackage](http://hackage.haskell.org/package/sshtun)
- Download the cabalized source tarball [from here](http://ui3.info/d/proj/sshtun/sshtun-1.0.0.tar.gz)
- Get the source with darcs: `$ darcs get http://hub.darcs.net/dino/sshtun`
- If you're just looking, [browse the source](http://hub.darcs.net/dino/sshtun)

And once you have it, building the usual way:

    $ cabal configure
    $ cabal build
    $ cabal install


## Installing

Build and install with cabal-install:
  `$ cabal update ; cabal install sshtun`


## Contact

Dino Morelli <[dino@ui3.info](mailto:dino@ui3.info)>
