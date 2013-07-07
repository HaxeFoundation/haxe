/*
 * Copyright (C)2005-2013 Haxe Foundation
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

/** <code>AnimationEvent</code> objects provide information about events that occur related to <a rel="internal" href="/api/js/html/Animation" title="en/CSS/CSS_animations">CSS animations</a>.<br><br>
Documentation for this class was provided by <a href="https://developer.mozilla.org/en/DOM/event/AnimationEvent">MDN</a>. */
@:native("AnimationEvent")
extern class AnimationEvent extends Event
{
	/** The name of the animation on which the animation event occurred. */
	var animationName(default,null) : String;

	/** The amount of time, in seconds, the animation had been running at the time the event occurred. */
	var elapsedTime(default,null) : Float;

	function new(type : String, canBubble : Bool = true, cancelable : Bool = true) : Void;

}
