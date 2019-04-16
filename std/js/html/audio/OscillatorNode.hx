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

// This file is generated from mozilla\OscillatorNode.webidl. Do not edit!

package js.html.audio;

/**
	The `OscillatorNode` interface represents a periodic waveform, such as a sine wave. It is an `AudioScheduledSourceNode` audio-processing module that causes a specified frequency of a given wave to be created—in effect, a constant tone.

	Documentation [OscillatorNode](https://developer.mozilla.org/en-US/docs/Web/API/OscillatorNode) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/OscillatorNode$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/OscillatorNode>
**/
@:native("OscillatorNode")
extern class OscillatorNode extends AudioScheduledSourceNode {
	
	/**
		A string which specifies the shape of waveform to play; this can be one of a number of standard values, or `custom` to use a `PeriodicWave` to describe a custom waveform. Different waves will produce different tones. Standard values are `"sine"`, `"square"`, `"sawtooth"`, `"triangle"` and `"custom"`. The default is `"sine"`.
	**/
	var type : OscillatorType;
	
	/**
		An a-rate `AudioParam` representing the frequency of oscillation in hertz (though the AudioParam` returned is read-only, the value it represents is not). The default value is 440 Hz (a standard middle-A note).
	**/
	var frequency(default,null) : AudioParam;
	
	/**
		An a-rate `AudioParam` representing detuning of oscillation in cents (though the AudioParam` returned is read-only, the value it represents is not). The default value is 0.
	**/
	var detune(default,null) : AudioParam;
	
	/** @throws DOMError */
	function new( context : BaseAudioContext, ?options : OscillatorOptions ) : Void;
	
	/**
		Sets a `PeriodicWave` which describes a periodic waveform to be used instead of one of the standard waveforms; calling this sets the `type` to `custom`. This replaces the now-obsolete `OscillatorNode.setWaveTable()` method.
	**/
	function setPeriodicWave( periodicWave : PeriodicWave ) : Void;
}