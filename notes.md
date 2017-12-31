# Development notes

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
- another UI feature: a "sum" object

## bugs

- You can get a variable on the rhs of its expression
- I sometimes get non-simplified stuff when I solve?
- "a - b" is rendered as "a + -1b"
- you can get "v = v" as an expression result
- Units are calculated incorrectly sometimes


## improvements

- allow dimensionless numbers
- [x] dragging from expr var to number should attach it
- Visual feedback for when you've successfully dragged onto something
  - just make it easier to drag on
    - THIS IS VERY IMPORTANT
- Maybe I should build an automated test suite using that "PossibleActions" code that I had but deleted
- If equations have a value, they should display it.
- Be smarter about calculating numerical values--search harder for answers.
- key shortcuts
- Refuse to let you drag equations out of the equation arena.
- Nicer number printing
    - If the value has no more precision, stop printing zeros

## Build notes

    time sbt clean fastOptJS; and cp target/scala-2.12/scala-gem-fastopt.j* web/gem-frontend/public/

## Equations to add

- Conversion between frequency and wavelength for light
- areas, volumes
- escape velocity


## Mistakes I've made

- Forgetting to turn a Set into a List before mapping over it with a non-one-to-one function
- Forgetting to add new things to the front of the * definition for Expression, leading to stack overflows 
