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

// This file is generated from mozilla\KeyframeEffect.webidl. Do not edit!

package js.html;

/**
	The `KeyframeEffectReadOnly` interface of the Web Animations API describes sets of animatable properties and values that can be played using the `Animation.Animation()` constructor, and which are inherited by `KeyframeEffect`.

	Documentation [KeyframeEffectReadOnly](https://developer.mozilla.org/en-US/docs/Web/API/KeyframeEffectReadOnly) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/KeyframeEffectReadOnly$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/KeyframeEffectReadOnly>
**/
@:native("KeyframeEffectReadOnly")
extern class KeyframeEffectReadOnly extends AnimationEffectReadOnly
{
	
	/**
		The element or pseudo-element being animated by this object. This may be `null` for animations that do not target a specific element.
	**/
	var target(default,null) : haxe.extern.EitherType<Element,CSSPseudoElement>;
	
	/**
		The iteration composite operation for resolving the property value changes of this keyframe effect.
	**/
	var iterationComposite(default,null) : IterationCompositeOperation;
	
	/**
		The composite operation property for resolving the property value changes between this and other keyframe effects.
	**/
	var composite(default,null) : CompositeOperation;
	
	/**
		The temporal spacing of the keyframe effect's iterations
	**/
	var spacing(default,null) : String;
	
	/** @throws DOMError */
	function new( target : haxe.extern.EitherType<Element,CSSPseudoElement>, frames : Dynamic, ?options : haxe.extern.EitherType<Float,Dynamic/*MISSING KeyframeEffectOptions*/> ) : Void;
	/** @throws DOMError */
	function getFrames() : Array<Dynamic>;
}