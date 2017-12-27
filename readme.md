# graphical equation manipulator, Scala edition

I had the idea for this software in 2013, but I wasn't a good enough programmer to make it. You can see my first attempt
on YouTube [here](https://www.youtube.com/watch?v=16eiGLrX248)
(featuring an Australian accent much stronger than the one I have after living in the US for a few years). Matt Alger then
[rewrote some of it in Coffeescript](https://github.com/MatthewJA/Graphical-Equation-Manipulator).

I'm going to do it in Scala, because type systems make it much easier for me to think about what I'm doing.

Features to add:

## [x] Phase 1: build out more backend

- Add numbers to the workspace
  - each has an optional variable it's connected to; these must be unique
- Workspace should expose a list of possible actions:
  - equality-declaration
  - expression-creation
  - expression-rewriting
  - number-creating
  - number-connecting
  - all deletions
- Equations should know more information about themselves, such as
  - name of equation
  - name for every variable
  - dimensions of every variable
- Logic to assign subscripts to every variable to remove ambiguity
- Logic to represent:
  - Equations as strings. This involves giving them a new field
  - Expressions as strings
- Computer algebra system

## [x] Phase 2: build shitty frontend

Just make logic for displaying everything and also a button for every allowed action


## [ ] Phase 3: make it less shitty

- less shitty GUI
  - similar to last time should work
    - I think I should just give up on using KaTeX
- let you search for equations

steps:

- Make my own rendering for equations.
  - Use the square root character for roots
  - Display fractions properly

## later

- magic triangle GUI element
- make parsers for equations and dimensions and stuff


## bugs

- You can get a variable on the rhs of its expression
- Deleting equations is a shitshow.
- Numbers aren't on the LHS of products
- The maps that I use will assign the same ID to multiple numbers.
    - To get around this I should make a "IdAssigningMap" class
- I sometimes get absurd stuff when I try to solve?

## improvements

- toString methods currently suck
- use minimum spanning tree to display equalities
- display lines that don't start from before the variables
- when rewriting equations, blank out equations that can't be used

## Build notes

    time sbt clean fastOptJS; and cp target/scala-2.12/scala-gem-fastopt.j* web/gem-frontend/public/
