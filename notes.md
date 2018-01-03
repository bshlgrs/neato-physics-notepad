# Development notes

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

## Features to add

- Most important: letting you make and share notepads. This requires:
  - JSON serialization
  - Setting title and adding comment text to the notepad
  - Web backend
- Vector semantics
- Diagrams
- Smarter expression calculations
  - smarter numerical expression calculations
- Differentiation
- Building differences/sums
- Crowdsourced equations

