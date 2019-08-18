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

// This file is generated from mozilla\AudioParam.webidl. Do not edit!

package js.html.audio;

/**
	The Web Audio API's `AudioParam` interface represents an audio-related parameter, usually a parameter of an `AudioNode` (such as `GainNode.gain`).

	Documentation [AudioParam](https://developer.mozilla.org/en-US/docs/Web/API/AudioParam) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/AudioParam$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/AudioParam>
**/
@:native("AudioParam")
extern class AudioParam {
	
	/**
		Represents the parameter's current volume as a floating point value; initially set to the value of `AudioParam.defaultValue`. Though it can be set, any modifications happening while there are automation events scheduled — that is events scheduled using the methods of the `AudioParam` — are ignored, without raising any exception.
	**/
	var value : Float;
	
	/**
		Represents the initial volume of the attribute as defined by the specific `AudioNode` creating the `AudioParam`.
	**/
	var defaultValue(default,null) : Float;
	
	/**
		Represents the minimum possible value for the parameter's nominal (effective) range. 
	**/
	var minValue(default,null) : Float;
	
	/**
		Represents the maximum possible value for the parameter's nominal (effective) range. 
	**/
	var maxValue(default,null) : Float;
	
	
	/**
		Schedules an instant change to the value of the `AudioParam` at a precise time, as measured against `AudioContext.currentTime`. The new value is given by the `value` parameter.
		@throws DOMError
	**/
	function setValueAtTime( value : Float, startTime : Float ) : AudioParam;
	
	/**
		Schedules a gradual linear change in the value of the `AudioParam`. The change starts at the time specified for the previous event, follows a linear ramp to the new value given in the `value` parameter, and reaches the new value at the time given in the `endTime` parameter.
		@throws DOMError
	**/
	function linearRampToValueAtTime( value : Float, endTime : Float ) : AudioParam;
	
	/**
		Schedules a gradual exponential change in the value of the `AudioParam`. The change starts at the time specified for the previous event, follows an exponential ramp to the new value given in the `value` parameter, and reaches the new value at the time given in the `endTime` parameter.
		@throws DOMError
	**/
	function exponentialRampToValueAtTime( value : Float, endTime : Float ) : AudioParam;
	
	/**
		Schedules the start of a change to the value of the `AudioParam`. The change starts at the time specified in `startTime` and exponentially moves towards the value given by the `target` parameter. The exponential decay rate is defined by the `timeConstant` parameter, which is a time measured in seconds.
		@throws DOMError
	**/
	function setTargetAtTime( target : Float, startTime : Float, timeConstant : Float ) : AudioParam;
	
	/**
		Schedules the values of the `AudioParam` to follow a set of values, defined by an array of floating-point numbers scaled to fit into the given interval, starting at a given start time and spanning a given duration of time.
		@throws DOMError
	**/
	function setValueCurveAtTime( values : Array<Float>, startTime : Float, duration : Float ) : AudioParam;
	
	/**
		Cancels all scheduled future changes to the `AudioParam`.
		@throws DOMError
	**/
	function cancelScheduledValues( startTime : Float ) : AudioParam;
}