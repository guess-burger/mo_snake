MO_Snake
========

A simple example of a online multiplayer version the classic game Snake.

TODO
====

- Create a ws **handler** that simply send an "echo" message for a basic snake
    - A 3 length snake in the centre of the board
    - What is the format of msgs going to be?
        - {dir, colour, [(1,2),(1,3),(1,4)]}?
        - the list can be compressed later on (head point, 4 left, 2 up, 3, r)
- Create another snake that listens to the same input
    - Make them different colours
- Make other snakes controlled by another client
    - any new client connecting will add a snake
    - no need to do an collision detection yet
    - will that need limiting at some point?
    - some nice random colour generation? golden angles etc
- Collisions?
    - Are game states needed then? Waiting, game & end?

These could all be git tags just keep track of the changes. Might be nice
