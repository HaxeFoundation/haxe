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

// This file is generated from mozilla\AudioNode.webidl. Do not edit!

package js.html.audio;

/**
	The `AudioNodeOptions` dictionary of the Web Audio API specifies options that can be used when creating new `AudioNode` objects.

	Documentation [AudioNodeOptions](https://developer.mozilla.org/en-US/docs/Web/API/AudioNodeOptions) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/AudioNodeOptions$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/AudioNodeOptions>
**/
typedef AudioNodeOptions = {
	
	/**
		Represents an integer used to determine how many channels are used when up-mixing and down-mixing connections to any inputs to the node. (See `AudioNode.channelCount` for more information.) Its usage and precise definition depend on the value of `AudioNodeOptions.channelCountMode`.
	**/
	var ?channelCount : Int;
	
	/**
		Represents an enumerated value describing the way channels must be matched between the node's inputs and outputs. (See `AudioNode.channelCountMode` for more information including default values.)
	**/
	var ?channelCountMode : ChannelCountMode;
	
	/**
		Represents an enumerated value describing the meaning of the channels. This interpretation will define how audio up-mixing and down-mixing will happen.
		
			The possible values are `"speakers"` or `"discrete"`. (See `AudioNode.channelCountMode` for more information including default values.)
	**/
	var ?channelInterpretation : ChannelInterpretation;
}