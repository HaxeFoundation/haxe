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

// This file is generated from mozilla\AudioDestinationNode.webidl. Do not edit!

package js.html.audio;

/**
	`AudioDestinationNode` has no output (as it is the output, no more `AudioNode` can be linked after it in the audio graph) and one input. The amount of channels in the input must be between `0` and the `maxChannelCount` value or an exception is raised.

	Documentation [AudioDestinationNode](https://developer.mozilla.org/en-US/docs/Web/API/AudioDestinationNode) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/AudioDestinationNode$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/AudioDestinationNode>
**/
@:native("AudioDestinationNode")
extern class AudioDestinationNode extends AudioNode
{
	
	/**
		Is an `unsigned long` defining the maximum amount of channels that the physical device can handle.
	**/
	var maxChannelCount(default,null) : Int;
	
}