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

// This file is generated from mozilla\ConstantSourceNode.webidl. Do not edit!

package js.html.audio;

/**
	The `ConstantSourceNode` interface—part of the Web Audio API—represents an audio source (based upon `AudioScheduledSourceNode`) whose output is single unchanging value. This makes it useful for cases in which you need a constant value coming in from an audio source. In addition, it can be used like a constructible `AudioParam` by automating the value of its `offset` or by connecting another node to it; see Controlling multiple parameters with ConstantSourceNode.

	Documentation [ConstantSourceNode](https://developer.mozilla.org/en-US/docs/Web/API/ConstantSourceNode) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/ConstantSourceNode$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/ConstantSourceNode>
**/
@:native("ConstantSourceNode")
extern class ConstantSourceNode extends AudioScheduledSourceNode {
	
	/**
		An `AudioParam` which specifies the value that this source continuously outputs. The default value is 1.0.
	**/
	var offset(default,null) : AudioParam;
	
	/** @throws DOMError */
	function new( context : BaseAudioContext, ?options : ConstantSourceOptions ) : Void;
}