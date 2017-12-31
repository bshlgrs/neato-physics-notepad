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


## [x] Phase 3: make it less shitty

- less shitty GUI
  - similar to last time should work
    - I think I should just give up on using KaTeX
- let you search for equations

## Current stuff

- render more prettily
  - this could involve fractions and surds
  - I am pretty sure I could build surds out of SVG
- parse equations more nicely?

Then try to do a bunch of the problems I claim I'm able to do, which involves adding a bunch of equations, and reassess.

Then, in this order:

- Creating equations
  - requires a parser. FastParse?
  - requires specifying units, maybe
    - I could just have equations be dynamically typed: they object if you try something unreasonable but are otherwise chill.
      - I think this is probably the way to go.
- Nonstandard units
  - I think this is just a small change to dimension parsing: let dimensions be `PhysicalNumber`s instead of `Dimension`s.
  - Numbers are displayed according to how they were input. Perhaps there's a toggle in the number menu to convert to different units.
- Triangle diagrams
- Differentiation
  - just differentiate F=ma to dF/dM = dM a or whatever.
  - This is an operation you can do on expressions
    - I think that these should not be rewritten

Also:

- Make my own rendering for equations.
  - Use the square root character for roots
  - Display fractions properly
- make a web backend

## later

- magic triangle GUI element
- make parsers for equations and dimensions and stuff

## bugs

- Reattaching numbers doesn't work
- Deleting equations reveals lots of bugs
- You can get a variable on the rhs of its expression
- The maps that I use will assign the same ID to multiple numbers.
    - To get around this I should make a "IdAssigningMap" class
- I sometimes get non-simplified stuff when I solve?
- "a - b" is rendered as "a + -1b"
- the nondeterministic ordering of items in a rendering of equation make it flicker
- you can get "v = v" as an expression result


## improvements

- [x] toString methods currently suck
- [x] use minimum spanning tree to display equalities
- [x] display lines that don't start from before the variables
- when rewriting equations, blank out equations that can't be used
  - Also rewriting sometimes doesn't work
- allow dimensionless numbers
- [x] dragging from expr var to number should attach it
- Visual feedback for when you've successfully dragged onto something
- Maybe I should build an automated test suite using that "PossibleActions" code that I had but deleted
- If equations have a value, they should display it.
- Be smarter about calculating numerical values.
- key shortcuts


## Build notes

    time sbt clean fastOptJS; and cp target/scala-2.12/scala-gem-fastopt.j* web/gem-frontend/public/

