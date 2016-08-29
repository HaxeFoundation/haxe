/*
 * Copyright (C)2005-2016 Haxe Foundation
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

// This file is generated from mozilla\Animation.webidl line 20:0. Do not edit!

package js.html;

@:native("Animation")
extern class Animation extends EventTarget
{
	var id : String;
	var effect(default,null) : AnimationEffectReadOnly;
	var timeline(default,null) : AnimationTimeline;
	var startTime : Float;
	var currentTime : Float;
	var playbackRate : Float;
	var playState(default,null) : AnimationPlayState;
	var ready(default,null) : Promise<Animation>;
	var finished(default,null) : Promise<Animation>;
	var onfinish : haxe.Constraints.Function;
	var oncancel : haxe.Constraints.Function;
	
	/** @throws DOMError */
	function new( ?effect : KeyframeEffectReadOnly, ?timeline : AnimationTimeline ) : Void;
	function cancel() : Void;
	/** @throws DOMError */
	function finish() : Void;
	/** @throws DOMError */
	function play() : Void;
	/** @throws DOMError */
	function pause() : Void;
	/** @throws DOMError */
	function reverse() : Void;
}