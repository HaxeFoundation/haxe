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

// This file is generated from mozilla\AnimationEffectTimingReadOnly.webidl. Do not edit!

package js.html;

/**
	The `AnimationEffectTimingReadOnly` interface of the Web Animations API is comprised of timing properties.

	Documentation [AnimationEffectTimingReadOnly](https://developer.mozilla.org/en-US/docs/Web/API/AnimationEffectTimingReadOnly) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/AnimationEffectTimingReadOnly$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/AnimationEffectTimingReadOnly>
**/
@:native("AnimationEffectTimingReadOnly")
extern class AnimationEffectTimingReadOnly
{
	
	/**
		The number of milliseconds to delay the start of the animation. Defaults to `0`.
	**/
	var delay(default,null) : Float;
	
	/**
		The number of milliseconds to delay after the end of an animation. This is primarily of use when sequencing animations based on the end time of another animation. Defaults to `0`.
	**/
	var endDelay(default,null) : Float;
	
	/**
		Dictates whether the animation's effects should be reflected by the element(s) state prior to playing (`backwards`), retained after the animation has completed playing (`forwards`), or `both`. Defaults to `none`.
	**/
	var fill(default,null) : FillMode;
	
	/**
		A number representing which repetition the animation begins at and its progress through it.
	**/
	var iterationStart(default,null) : Float;
	
	/**
		The number of times the animation should repeat. Defaults to `1`, and can also take a value of infinity to make it repeat infinitely.
	**/
	var iterations(default,null) : Float;
	
	/**
		The number of milliseconds each iteration of the animation takes to complete. Defaults to `0`.
	**/
	var duration(default,null) : haxe.extern.EitherType<Float,String>;
	
	/**
		Whether the animation runs forwards (`normal`), backwards (`reverse`), switches direction after each iteration (`alternate`), or runs backwards and switches direction after each iteration (`alternate-reverse`). Defaults to `normal`.
	**/
	var direction(default,null) : PlaybackDirection;
	
	/**
		The rate of the animation's change over time. Accepts the pre-defined values `linear`, `ease`, `ease-in`, `ease-out`, and `ease-in-out`, or a custom cubic-bezier value like `cubic-bezier(0.42, 0, 0.58, 1)`. Defaults to `linear`.
	**/
	var easing(default,null) : String;
	
}