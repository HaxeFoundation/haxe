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

// This file is generated from mozilla\AudioNode.webidl. Do not edit!

package js.html.audio;

/**
	The `AudioNode` interface is a generic interface for representing an audio processing module like an audio source (e.g. an HTML `audio` or `video` element, an `OscillatorNode`, etc.), the audio destination, intermediate processing module (e.g. a filter like `BiquadFilterNode` or `ConvolverNode`), or volume control (like `GainNode`).

	Documentation [AudioNode](https://developer.mozilla.org/en-US/docs/Web/API/AudioNode) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/AudioNode$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/AudioNode>
**/
@:native("AudioNode")
extern class AudioNode extends js.html.EventTarget
{
	
	/**
		Returns the associated `AudioContext`, that is the object representing the processing graph the node is participating in.
	**/
	var context(default,null) : AudioContext;
	
	/**
		Returns the number of inputs feeding the node. Source nodes are defined as nodes having a `numberOfInputs` property with a value of `0`.
	**/
	var numberOfInputs(default,null) : Int;
	
	/**
		Returns the number of outputs coming out of the node. Destination nodes — like `AudioDestinationNode` — have a value of `0` for this attribute.
	**/
	var numberOfOutputs(default,null) : Int;
	
	/**
		Represents an integer used to determine how many channels are used when up-mixing and down-mixing connections to any inputs to the node. Its usage and precise definition depend on the value of `AudioNode.channelCountMode`.
	**/
	var channelCount : Int;
	
	/**
		Represents an enumerated value describing the way channels must be matched between the node's inputs and outputs.
	**/
	var channelCountMode : ChannelCountMode;
	
	/**
		Represents an enumerated value describing the meaning of the channels. This interpretation will define how audio up-mixing and down-mixing will happen.
		
		 The possible values are `"speakers"` or `"discrete"`.
	**/
	var channelInterpretation : ChannelInterpretation;
	
	/** @throws DOMError */
	@:overload( function( destination : AudioNode, ?output : Int = 0, ?input : Int = 0 ) : AudioNode {} )
	
	/**
		Allows us to connect one output of this node to one input of an audio parameter.
	**/
	function connect( destination : AudioParam, ?output : Int = 0 ) : Void;
	/** @throws DOMError */
	
	/**
		Allows us to disconnect the current node from another one it is already connected to.
	**/
	function disconnect( ?output : Int = 0 ) : Void;
}