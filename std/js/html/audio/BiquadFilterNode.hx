/*
 * Copyright (C)2005-2013 Haxe Foundation
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

// This file is generated, do not edit!
package js.html.audio;

@:native("BiquadFilterNode")
extern class BiquadFilterNode extends AudioNode
{
	static inline var ALLPASS : Int = 7;

	static inline var BANDPASS : Int = 2;

	static inline var HIGHPASS : Int = 1;

	static inline var HIGHSHELF : Int = 4;

	static inline var LOWPASS : Int = 0;

	static inline var LOWSHELF : Int = 3;

	static inline var NOTCH : Int = 6;

	static inline var PEAKING : Int = 5;

	var Q(default,null) : AudioParam;

	var frequency(default,null) : AudioParam;

	var gain(default,null) : AudioParam;

	/** Setter throws DOMException. */
	var type : Int;

	function getFrequencyResponse( frequencyHz : js.html.Float32Array, magResponse : js.html.Float32Array, phaseResponse : js.html.Float32Array ) : Void;

}
