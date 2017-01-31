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

// This file is generated from mozilla\ConvolverNode.webidl. Do not edit!

package js.html.audio;

/**
	The `ConvolverNode` interface is an `AudioNode` that performs a Linear Convolution on a given `AudioBuffer`, often used to achieve a reverb effect. A `ConvolverNode` always has exactly one input and one output.

	Documentation [ConvolverNode](https://developer.mozilla.org/en-US/docs/Web/API/ConvolverNode) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/ConvolverNode$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/ConvolverNode>
**/
@:native("ConvolverNode")
extern class ConvolverNode extends AudioNode
{
	
	/**
		A mono, stereo, or 4-channel `AudioBuffer` containing the (possibly multichannel) impulse response used by the `ConvolverNode` to create the reverb effect.
	**/
	var buffer : AudioBuffer;
	
	/**
		A boolean that controls whether the impulse response from the buffer will be scaled by an equal-power normalization when the `buffer` attribute is set, or not.
	**/
	var normalize : Bool;
	
}