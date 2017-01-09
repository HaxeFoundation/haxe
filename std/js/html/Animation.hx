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

// This file is generated from mozilla\Animation.webidl. Do not edit!

package js.html;

/**
	The `Animation` interface of the Web Animations API represents a single animation player and provides playback controls and a timeline for an animation node or source.

	Documentation [Animation](https://developer.mozilla.org/en-US/docs/Web/API/Animation) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/Animation$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/Animation>
**/
@:native("Animation")
extern class Animation extends EventTarget
{
	
	/**
		Gets and sets the `String` used to identify the animation.
	**/
	var id : String;
	
	/**
		Gets and sets the `AnimationEffectReadOnly` associated with this animation. This will usually be a `KeyframeEffect` object.
	**/
	var effect(default,null) : AnimationEffectReadOnly;
	
	/**
		Gets or sets the `AnimationTimeline` associated with this animation.
	**/
	var timeline(default,null) : AnimationTimeline;
	
	/**
		Gets or sets the scheduled time when an animation's playback should begin.
	**/
	var startTime : Float;
	
	/**
		The current time value of the animation in milliseconds, whether running or paused. If the animation lacks a `AnimationTimeline`, is inactive or hasn't been played yet, its value is `null`.
	**/
	var currentTime : Float;
	
	/**
		Gets or sets the playback rate of the animation.
	**/
	var playbackRate : Float;
	
	/**
		Returns an enumerated value describing the playback state of an animation.
	**/
	var playState(default,null) : AnimationPlayState;
	
	/**
		Returns the current ready Promise for this animation.
	**/
	var ready(default,null) : Promise<Animation>;
	
	/**
		Returns the current finished Promise for this animation.
	**/
	var finished(default,null) : Promise<Animation>;
	
	/**
		Gets and sets the event handler for the `finish` event.
	**/
	var onfinish : haxe.Constraints.Function;
	
	/**
		Gets and sets the event handler for the `cancel` event.
	**/
	var oncancel : haxe.Constraints.Function;
	
	/** @throws DOMError */
	function new( ?effect : KeyframeEffectReadOnly, ?timeline : AnimationTimeline ) : Void;
	
	/**
		Clears all `KeyframeEffect` caused by this animation and aborts its playback.
	**/
	function cancel() : Void;
	/** @throws DOMError */
	
	/**
		Seeks either end of an animation, depending on whether the animation is playing or reversing.
	**/
	function finish() : Void;
	/** @throws DOMError */
	
	/**
		Starts or resumes playing of an animation, or begins the animation again if it previously finished.
	**/
	function play() : Void;
	/** @throws DOMError */
	
	/**
		Suspends playing of an animation.
	**/
	function pause() : Void;
	/** @throws DOMError */
	
	/**
		Reverses playback direction, stopping at the start of the animation. If the animation is finished or unplayed, it will play from end to beginning.
	**/
	function reverse() : Void;
}