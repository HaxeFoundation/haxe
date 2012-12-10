/*
 * Copyright (C)2005-2012 Haxe Foundation
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 */

// This file is generated, do not edit!
package js.html;

/** <p>CSS&nbsp;animations make it possible to animate transitions from one CSS style configuration to another. Animations consist of two components:&nbsp;A style describing the animation and a set of keyframes that indicate the start and end states of the animation's CSS style, as well as possible intermediate waypoints along the way.</p>
<p>There are three key advantages to CSS&nbsp;animations over traditional script-driven animation techniques:</p>
<ol> <li>They're easy to use for simple animations; you can create them without even having to know JavaScript.</li> <li>The animations run well, even under moderate system load. Simple animations can often perform poorly in JavaScript (unless they're well made). The rendering engine can use frame-skipping and other techniques to keep the performance as smooth as possible.</li> <li>Letting the browser control the animation sequence lets the browser optimize performance and efficiency by, for example, reducing the update frequency of animations running in tabs that aren't currently visible.</li>
</ol><br><br>
Documentation for this class was provided by <a href="https://developer.mozilla.org/en/CSS/CSS_animations">MDN</a>. */
@:native("Animation")
extern class Animation
{
    static inline var DIRECTION_ALTERNATE :Int = 1;

    static inline var DIRECTION_NORMAL :Int = 0;

    static inline var FILL_BACKWARDS :Int = 1;

    static inline var FILL_BOTH :Int = 3;

    static inline var FILL_FORWARDS :Int = 2;

    static inline var FILL_NONE :Int = 0;

    var delay (default,null) :Float;

    var direction (default,null) :Int;

    var duration (default,null) :Float;

    var elapsedTime :Float;

    var ended (default,null) :Bool;

    var fillMode (default,null) :Int;

    var iterationCount (default,null) :Int;

    var name (default,null) :String;

    var paused (default,null) :Bool;

    function pause () :Void;

    function play () :Void;

}
