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

// This file is generated from mozilla\AnimationTimeline.webidl. Do not edit!

package js.html;

/**
	The `AnimationTimeline` interface of the Web Animations API represents the timeline of an animation. This interface exists to define timeline features (inherited by `DocumentTimeline` and future timeline types) and is not itself directly used by developers. Anywhere you see `AnimationTimeline`, you should use `DocumentTimeline` or any other timeline type instead.

	Documentation [AnimationTimeline](https://developer.mozilla.org/en-US/docs/Web/API/AnimationTimeline) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/AnimationTimeline$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/AnimationTimeline>
**/
@:native("AnimationTimeline")
extern class AnimationTimeline
{
	
	/**
		Returns the time value in milliseconds for this timeline or `null` if this timeline is inactive.
	**/
	var currentTime(default,null) : Float;
	
}