# sshtun


## Synopsis

Wrapper daemon to manage an ssh tunnel (Haskell)


## Description

This is a daemon that executes an ssh command to form a secure
tunnel and then blocks on it. If the tunnel goes down, sshtun will
attempt to reestablish it. It can also be set up to monitor a file
on an http server to determine if the tunnel should be up or not,
so you can switch it on or off remotely.

It's this last behavior, the switching on and off remotely, that makes this
project differ from the [simpler sshtun shell
script](https://github.com/dino-/scripts/blob/master/sshtun) I wrote. The
switching behavior together with blocking on the tunnel requires separate
threads and so I attacked the problem with Haskell.


## Getting source

- Download the cabalized source package [from Hackage](http://hackage.haskell.org/package/sshtun)
- Get the source with darcs: `$ git clone https://github.com/dino-/sshtun.git`
- If you're just looking, [browse the source](https://github.com/dino-/sshtun)

And once you have it, building the usual way:

    $ cabal configure
    $ cabal build
    $ cabal install


## Installing

Build and install with cabal-install:
  `$ cabal update ; cabal install sshtun`


## Contact

Dino Morelli <[dino@ui3.info](mailto:dino@ui3.info)>
