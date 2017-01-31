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

// This file is generated from mozilla\AnimationEffectReadOnly.webidl. Do not edit!

package js.html;

/**
	The `AnimationEffectReadOnly` interface of the Web Animations API defines current and future animation effects like `KeyframeEffect`, which can be passed to `Animation` objects for playing, and `KeyframeEffectReadOnly` (which is used by CSS Animations and Transitions).

	Documentation [AnimationEffectReadOnly](https://developer.mozilla.org/en-US/docs/Web/API/AnimationEffectReadOnly) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/AnimationEffectReadOnly$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/AnimationEffectReadOnly>
**/
@:native("AnimationEffectReadOnly")
extern class AnimationEffectReadOnly
{
	
	/**
		The `AnimationEffectTimingReadOnly` object associated with the animation containing all the animation's timing values.
	**/
	var timing(default,null) : AnimationEffectTimingReadOnly;
	
	
	/**
		Returns the calculated timing properties for this Animation Effect.
	**/
	function getComputedTiming() : ComputedTimingProperties;
}