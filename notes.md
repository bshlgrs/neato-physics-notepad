# Development notes


## Build notes

    time sbt clean fastOptJS; and cp target/scala-2.12/scala-gem-fastopt.j* web/gem-frontend/public/

## Equations to add

- areas, volumes
- escape velocity
- second rocket equation
    - also delete the spurious first one
- E = 1 / (2* π * ε_0) * p/(z**3)

## Mistakes I've made

- Forgetting to turn a Set into a List before mapping over it with a non-one-to-one function
- Forgetting to add new things to the front of the * definition for Expression, leading to stack overflows

## Features to add

- More backend features
- Vector: You should be able to add onto the page diagrams that represent geometric facts about a situation. For example, you should be able to add a “Right-angled triangle diagram”, which is represented as a right-angle triangle. The user can treat its vertices and edges as variables and drag other variables to those parts of the triangle to declare those equal, just like you normally do by dragging to other variables. This would allow you to solve problems which require a few steps of trig.
- Diagrams
  - eg free body diagrams
- Smarter expression calculations
  - smarter numerical expression calculations
- Differentiation
- Building differences/sums
- Crowdsourced equations
- Quadratic equations
- Built-in numbers. It would be nice to be able to search for “mass of sun” or “charge of electron” in the equation search thing and have that get you the result.
- Smarter inference of dimensions
