/*
 * Copyright (C)2005-2017 Haxe Foundation
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

// This file is generated from mozilla\AnimationEvent.webidl. Do not edit!

package js.html;

/**
	The `AnimationEvent` interface represents events providing information related to animations.

	Documentation [AnimationEvent](https://developer.mozilla.org/en-US/docs/Web/API/AnimationEvent) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/AnimationEvent$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/AnimationEvent>
**/
@:native("AnimationEvent")
extern class AnimationEvent extends Event
{
	
	/**
		Is a `DOMString` containing the value of the `animation-name` CSS property associated with the transition.
	**/
	var animationName(default,null) : String;
	
	/**
		Is a `float` giving the amount of time the animation has been running, in seconds, when this event fired, excluding any time the animation was paused. For an `"animationstart"` event, `elapsedTime` is `0.0` unless there was a negative value for `animation-delay`, in which case the event will be fired with `elapsedTime` containing  `(-1 * `delay`)`.
	**/
	var elapsedTime(default,null) : Float;
	
	/**
		Is a `DOMString`, starting with `'::'`, containing the name of the pseudo-element the animation runs on. If the animation doesn't run on a pseudo-element but on the element, an empty string: `''``.`
	**/
	var pseudoElement(default,null) : String;
	
	/** @throws DOMError */
	function new( type : String, ?eventInitDict : AnimationEventInit ) : Void;
}