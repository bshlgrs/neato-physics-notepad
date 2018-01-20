import React from 'react';

const Help = () => <div>
    <h2>Help</h2>

    <p>Welcome to my Neato Physics Notepad! If you haven't seen this before, I recommend
    watching this video to see what it does:</p>

    <iframe width="560"
        height="315"
        src="https://www.youtube.com/embed/RWPHu8Vynv8?rel=0"
        frameborder="0"
        allow="autoplay; encrypted-media"
        allowfullscreen />

    <p>Here are the notepads from the video, so that you can play with them:</p>

    <ul>
        <li><a href="http://physics.shlegeris.com/notepads/5">Brick</a></li>
        <li><a href="http://physics.shlegeris.com/notepads/7">Charge passing through a resistor</a></li>
        <li><a href="http://physics.shlegeris.com/notepads/3">Particles released from rest</a></li>
    </ul>

    <p>The source code is on <a href="https://github.com/bshlgrs/neato-physics-notepad">Github</a>.</p>
  </div>

export default Help;
