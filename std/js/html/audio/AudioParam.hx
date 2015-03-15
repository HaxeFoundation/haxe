/*
 * Copyright (C)2005-2015 Haxe Foundation
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

// This file is generated from mozilla/AudioParam.webidl line 13:0. Do not edit!

package js.html.audio;

@:native("AudioParam")
extern class AudioParam
{
	var value : Float;
	var defaultValue(default,null) : Float;
	
	/** @throws DOMError */
	function setValueAtTime( value : Float, startTime : Float ) : Void;
	/** @throws DOMError */
	function linearRampToValueAtTime( value : Float, endTime : Float ) : Void;
	/** @throws DOMError */
	function exponentialRampToValueAtTime( value : Float, endTime : Float ) : Void;
	/** @throws DOMError */
	function setTargetAtTime( target : Float, startTime : Float, timeConstant : Float ) : Void;
	/** @throws DOMError */
	function setValueCurveAtTime( values : js.html.Float32Array, startTime : Float, duration : Float ) : Void;
	/** @throws DOMError */
	function cancelScheduledValues( startTime : Float ) : Void;
}