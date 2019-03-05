/*
 * Copyright (C)2005-2019 Haxe Foundation
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

// This file is generated from mozilla\AnimationEffect.webidl. Do not edit!

package js.html;

/**
	The `EffectTiming` dictionary, part of the Web Animations API, is used by `Element.animate()`, `KeyframeEffectReadOnly()`, and `KeyframeEffect()` to describe timing properties for animation effects. These properties are all optional, although without setting a `duration` the animation will not play.

	Documentation [EffectTiming](https://developer.mozilla.org/en-US/docs/Web/API/EffectTiming) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/EffectTiming$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/EffectTiming>
**/
typedef EffectTiming = {
	
	/**
		The number of milliseconds to delay the start of the animation. Defaults to 0.
	**/
	var ?delay : Float;
	
	/**
		Whether the animation runs forwards (`normal`), backwards (`reverse`), switches direction after each iteration (`alternate`), or runs backwards and switches direction after each iteration (`alternate-reverse`). Defaults to `"normal"`.
	**/
	var ?direction : PlaybackDirection;
	
	/**
		The number of milliseconds each iteration of the animation takes to complete. Defaults to 0. Although this is technically optional, keep in mind that your animation will not run if this value is 0.
	**/
	var ?duration : haxe.extern.EitherType<Float,String>;
	
	/**
		The rate of the animation's change over time. Accepts the pre-defined values `"linear"`, `"ease"`, `"ease-in"`, `"ease-out"`, and `"ease-in-out"`, or a custom `"cubic-bezier"` value like `"cubic-bezier(0.42, 0, 0.58, 1)"`. Defaults to `"linear"`.
	**/
	var ?easing : String;
	
	/**
		The number of milliseconds to delay after the end of an animation. This is primarily of use when sequencing animations based on the end time of another animation. Defaults to 0. 
	**/
	var ?endDelay : Float;
	
	/**
		Dictates whether the animation's effects should be reflected by the element(s) prior to playing (`"backwards"`), retained after the animation has completed playing (`"forwards"`), or `both`. Defaults to `"none"`.
	**/
	var ?fill : FillMode;
	
	/**
		Describes at what point in the iteration the animation should start. 0.5 would indicate starting halfway through the first iteration for example, and with this value set, an animation with 2 iterations would end halfway through a third iteration. Defaults to 0.0.
	**/
	var ?iterationStart : Float;
	
	/**
		The number of times the animation should repeat. Defaults to `1`, and can also take a value of `Infinity` to make it repeat for as long as the element exists.
	**/
	var ?iterations : Float;
}