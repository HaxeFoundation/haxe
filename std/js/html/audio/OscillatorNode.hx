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

// This file is generated from mozilla\OscillatorNode.webidl. Do not edit!

package js.html.audio;

/**
	The `OscillatorNode` interface represents a periodic waveform, like a sine wave. It is an `AudioNode` audio-processing module that causes a given frequency of sine wave to be created — in effect, a constant tone.

	Documentation [OscillatorNode](https://developer.mozilla.org/en-US/docs/Web/API/OscillatorNode) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/OscillatorNode$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/OscillatorNode>
**/
@:native("OscillatorNode")
extern class OscillatorNode extends AudioNode
{
	
	/**
		Represents the shape of the oscillator wave generated. Different waves will produce different tones.
	**/
	var type : OscillatorType;
	
	/**
		An a-rate `AudioParam` representing the frequency of oscillation in hertz (though the AudioParam` returned is read-only, the value it represents is not.)
	**/
	var frequency(default,null) : AudioParam;
	
	/**
		An a-rate `AudioParam` representing detuning of oscillation in cents (though the AudioParam` returned is read-only, the value it represents is not.)
	**/
	var detune(default,null) : AudioParam;
	var onended : haxe.Constraints.Function;
	
	/** @throws DOMError */
	
	/**
		This method specifies the exact time to start playing the tone.
	**/
	function start( ?when : Float = 0.0 ) : Void;
	/** @throws DOMError */
	
	/**
		This method specifies the exact time to stop playing the tone.
	**/
	function stop( ?when : Float = 0.0 ) : Void;
	
	/**
		Used to point to a `PeriodicWave` defining a periodic waveform that can be used to shape the oscillator's output, when `type = "custom"` is used. This replaces the now-obsolete `OscillatorNode.setWaveTable`.
	**/
	function setPeriodicWave( periodicWave : PeriodicWave ) : Void;
}