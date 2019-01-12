[![Alt text](https://img.youtube.com/vi/4mdXDXMSkAE/0.jpg)](https://www.youtube.com/watch?v=4mdXDXMSkAE)


# A Game built in Haskell with FRP

## Install

Requires SuperCollider and Tidal to be installed

```
cabal install
```

## Playing

```
cabal run TransGame
```

for now, after playing, you need to manually kill sclang, eg

```
mark@mark-PC:~/Github/FRPGame$ ps -A | grep sclang
11564 pts/0    00:00:01 sclang
mark@mark-PC:~/Github/FRPGame$ kill 11564
```

## Analysis

to then get the haskell profiling report

hp2ps -e8in -c TransGame


To split gifs (which we then rerender with FRP)
http://ezgif.com/split

To get the outline, using convert from imagemagick

   convert lightsOff.png -canny 0x1+10%+30% -negate lightsOutline.png
