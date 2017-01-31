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

// This file is generated from mozilla\DynamicsCompressorNode.webidl. Do not edit!

package js.html.audio;

/**
	Inherits properties from its parent, `AudioNode`.

	Documentation [DynamicsCompressorNode](https://developer.mozilla.org/en-US/docs/Web/API/DynamicsCompressorNode) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/DynamicsCompressorNode$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/DynamicsCompressorNode>
**/
@:native("DynamicsCompressorNode")
extern class DynamicsCompressorNode extends AudioNode
{
	
	/**
		Is a `AudioParam` representing the decibel value above which the compression will start taking effect.
	**/
	var threshold(default,null) : AudioParam;
	
	/**
		Is a `AudioParam` containing a decibel value representing the range above the threshold where the curve smoothly transitions to the compressed portion.
	**/
	var knee(default,null) : AudioParam;
	
	/**
		Is a `AudioParam` representing the amount of change, in dB, needed in the input for a 1 dB change in the output.
	**/
	var ratio(default,null) : AudioParam;
	
	/**
		Is a `float `representing the amount of gain reduction currently applied by the compressor to the signal.
	**/
	var reduction(default,null) : Float;
	
	/**
		Is a `AudioParam` representing the amount of time, in seconds, required to reduce the gain by 10 dB.
	**/
	var attack(default,null) : AudioParam;
	
	/**
		Is a `AudioParam` representing the amount of time, in seconds, required to increase the gain by 10 dB.
	**/
	var release(default,null) : AudioParam;
	
}