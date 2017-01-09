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

// This file is generated from mozilla\PannerNode.webidl. Do not edit!

package js.html.audio;

/**
	A `PannerNode` always has exactly one input and one output: the input can be mono or stereo but the output is always stereo (2 channels); you can't have panning effects without at least two audio channels!

	Documentation [PannerNode](https://developer.mozilla.org/en-US/docs/Web/API/PannerNode) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/PannerNode$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/PannerNode>
**/
@:native("PannerNode")
extern class PannerNode extends AudioNode
{
	
	/**
		An enumerated value determining which spatialisation algorithm to use to position the audio in 3D space.
	**/
	var panningModel : PanningModelType;
	
	/**
		An enumerated value determining which algorithm to use to reduce the volume of the audio source as it moves away from the listener.
	**/
	var distanceModel : DistanceModelType;
	
	/**
		A double value representing the reference distance for reducing volume as the audio source moves further from the listener.
	**/
	var refDistance : Float;
	
	/**
		A double value representing the maximum distance between the audio source and the listener, after which the volume is not reduced any further.
	**/
	var maxDistance : Float;
	
	/**
		A double value describing how quickly the volume is reduced as the source moves away from the listener. This value is used by all distance models.
	**/
	var rolloffFactor : Float;
	
	/**
		Is a double value describing the angle, in degrees, of a cone inside of which there will be no volume reduction.
	**/
	var coneInnerAngle : Float;
	
	/**
		A double value describing the angle, in degrees, of a cone outside of which the volume will be reduced by a constant value, defined by the `coneOuterGain` attribute.
	**/
	var coneOuterAngle : Float;
	
	/**
		A double value describing the amount of volume reduction outside the cone defined by the `coneOuterAngle` attribute. Its default value is `0`, meaning that no sound can be heard.
	**/
	var coneOuterGain : Float;
	
	
	/**
		Defines the position of the audio source relative to the listener (represented by an `AudioListener` object stored in the `AudioContext.listener` attribute.)
	**/
	function setPosition( x : Float, y : Float, z : Float ) : Void;
	
	/**
		Defines the direction the audio source is playing in.
	**/
	function setOrientation( x : Float, y : Float, z : Float ) : Void;
	
	/**
		Defines the velocity vector of the audio source — how fast it is moving and in what direction. In a previous version of the specification, the `PannerNode` had a velocity that could pitch up or down `AudioBufferSourceNode`s connected downstream. This feature was not clearly specified and had a number of issues, so it was removed from the specification.
	**/
	function setVelocity( x : Float, y : Float, z : Float ) : Void;
}