# Neato Physics Notepad

## Deployed [here](http://physics.shlegeris.com/). Full demo video [here](https://youtu.be/RWPHu8Vynv8).

![demo gif](./demo%20gif.gif)

A lot of the physics problems I encounter in my daily life, as well as a lot of the physics problems you get in physics classes 
up to second year university, follow a pretty consistent format. You get a problem like the one in the GIF above:

> A 2.3kg brick slides down a slope, ending up 4.2 meters lower than its starting point. Neglecting friction, how fast is it going when it gets to the bottom?

You say, okay, so I'm going to reason about this using energy conservation. The gravitational potential energy of the brick
at the top must equal its kinetic energy at the bottom. So let's write down those two equations:

```
KE = ½ m v²
PE = m g h
```

And we note to ourselves the two equality relationship here: the kinetic energy in the first equation equals the 
potential energy in the second, and the masses in the two equations are equal.

We solve the first equation for `v` and get `√(2 KE/m)`. This is fine, but I don't know the KE. Can we rewrite this in terms 
of things we do know? As it happens, we can--solve the second equation for PE and you get `m g h` (obviously), and now 
substitute that in to the expression for `v`, and you get `√(2 m g h/m)` which simplifies to `√(2 g h)`.

If we're being good, we stop for a second to sanity check this by dimensional analysis, which works out, and checking the
functional form--velocity gets bigger if the ramp is taller or gravity is stronger, which seems right.

That's the algebraic solution. Now we substitute in the numbers and then we get our answer, which is about 9m/s. 

The steps here were:

1. Write down the equations relevant to the situation.
2. Note which variables are equal between the equations.
3. Get expressions for particular variables of interest
4. Rewrite those expressions using the equations and equalities established in steps 1 and 2.
5. Check the dimensions and functional form
6. Substitute in numbers.

My Neato Physics Notebook is built around this workflow. To solve a problem, you search for the relevant equations, or 
write them down yourself if the software doesn't have them. Then you describe equalites between the equations. After 
you've done this, you can solve for variables and get their numerical values. As a bonus, this software makes it 
unnecessary to check dimensions, because the software doesn't let you thinks that aren't dimensionally reasonable.

There are a lot of physics problems that this software is totally useless for. But it's very useful for a large class of
problems.

## Usage notes

You should try to pick up the usage by looking at the GIF above. A few things to note:

- Currently I don't have that many physics equations in my library.
- You can only drag equations and expressions by their text--for example, you can't click and drag on the bar of a 
    fraction to drag it. Obviously I plan to fix this.

## Major things this is missing

- All equations are in scalars. This means that equations that are usually described in terms of vectors are quite 
    clunky to use. I plan to change this.
- It doesn't let you manipulate equations by differentiating them or integrating them.
- It can't help you with bits of trigonometry that come up. I plan to fix this in a neat way.
- Currently it can't solve quadratic equations.
- The most important and boring feature: I want this software to be easy to use as a notepad. For that to work, I need
    to give people the ability to save, share and fork the pages they make with this. I'm going to make this happen 
    really soon.


## Contributing

If you want to help me out by solving a bunch of physics problems with this software and adding equations to its library
as you go, that would be amazing.
