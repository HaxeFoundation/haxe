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

// This file is generated from mozilla\IIRFilterNode.webidl. Do not edit!

package js.html.audio;

/**
	The `IIRFilterNode` interface of the Web Audio API is a `AudioNode` processor which implements a general infinite impulse response (IIR)  filter; this type of filter can be used to implement tone control devices and graphic equalizers as well. It lets the parameters of the filter response be specified, so that it can be tuned as needed.

	Documentation [IIRFilterNode](https://developer.mozilla.org/en-US/docs/Web/API/IIRFilterNode) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/IIRFilterNode$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/IIRFilterNode>
**/
@:native("IIRFilterNode")
extern class IIRFilterNode extends AudioNode {
	/** @throws DOMError */
	function new( context : BaseAudioContext, options : IIRFilterOptions ) : Void;
	function getFrequencyResponse( frequencyHz : js.lib.Float32Array, magResponse : js.lib.Float32Array, phaseResponse : js.lib.Float32Array ) : Void;
}