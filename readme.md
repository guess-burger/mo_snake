MO_Snake
========

A simple example of a online multiplayer version the classic game Snake.

The is a 80/20 solution, with 80% of the features from 20% of the implementation. 
As such there are a lot of things that could be improved, see later.

Build and Running
-----------------
This project is built with a combination of [Erlang](https://www.erlang.org/) and [Elm](http://elm-lang.org/)  

### Erlang
This code was written for OTP 18 and built using [erlang.mk](https://github.com/ninenines/erlang.mk). 

Dependencies can be downloaded by running 'make deps'. Erlang code can be built by running `make app`

### Elm
Elm code was written for Elm 0.16.

Getting dependencies for elm was move involved than Erlang. 
Try elm-package but if that fails you may have to download the [the core 3.0.0](https://github.com/elm-lang/core/archive/3.0.0.tar.gz), untar it to `./priv/elm/elm-stuff/packages/elm-lang/`.
It can then be built by running `make elm`


### Running locally
Run with `make shell` and browse to [localhost:8080]().
While running you can modify Erlang and it will be recompiled and reloaded thanks to [sync](https://github.com/rustyio/sync)


Issues & Improvements
---------------------
Being a 80/20 solution there are a number of issues that could be improved.

* The UI is _very_ basic!
  * Everything including text is render as a snake. This was done for simplicity should really be changed
  * Everything is driven directly through websockets. Since there is no predictive movements lag can be an issue
* There is currently no limit to how many matches that can be started. Matches are also not tracked

