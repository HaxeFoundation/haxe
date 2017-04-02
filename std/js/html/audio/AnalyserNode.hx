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

// This file is generated from mozilla\AnalyserNode.webidl. Do not edit!

package js.html.audio;

/**
	The `AnalyserNode` interface represents a node able to provide real-time frequency and time-domain analysis information. It is an `AudioNode` that passes the audio stream unchanged from the input to the output, but allows you to take the generated data, process it, and create audio visualizations.

	Documentation [AnalyserNode](https://developer.mozilla.org/en-US/docs/Web/API/AnalyserNode) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/AnalyserNode$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/AnalyserNode>
**/
@:native("AnalyserNode")
extern class AnalyserNode extends AudioNode
{
	
	/**
		Is an unsigned long value representing the size of the FFT (Fast Fourier Transform) to be used to determine the frequency domain.
	**/
	var fftSize : Int;
	
	/**
		Is an unsigned long value half that of the FFT size. This generally equates to the number of data values you will have to play with for the visualization.
	**/
	var frequencyBinCount(default,null) : Int;
	
	/**
		Is a double value representing the minimum power value in the scaling range for the FFT analysis data, for conversion to unsigned byte values — basically, this specifies the minimum value for the range of results when using `getByteFrequencyData()`.
	**/
	var minDecibels : Float;
	
	/**
		Is a double value representing the maximum power value in the scaling range for the FFT analysis data, for conversion to unsigned byte values — basically, this specifies the maximum value for the range of results when using `getByteFrequencyData()`.
	**/
	var maxDecibels : Float;
	
	/**
		Is a double value representing the averaging constant with the last analysis frame — basically, it makes the transition between values over time smoother.
	**/
	var smoothingTimeConstant : Float;
	
	
	/**
		Copies the current frequency data into a `Float32Array` array passed into it.
	**/
	function getFloatFrequencyData( array : js.html.Float32Array ) : Void;
	
	/**
		Copies the current frequency data into a `Uint8Array` (unsigned byte array) passed into it.
	**/
	function getByteFrequencyData( array : js.html.Uint8Array ) : Void;
	
	/**
		Copies the current waveform, or time-domain, data into a `Float32Array` array passed into it.
	**/
	function getFloatTimeDomainData( array : js.html.Float32Array ) : Void;
	
	/**
		Copies the current waveform, or time-domain, data into a `Uint8Array` (unsigned byte array) passed into it.
	**/
	function getByteTimeDomainData( array : js.html.Uint8Array ) : Void;
}