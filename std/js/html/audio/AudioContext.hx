/*
 * Copyright (C)2005-2015 Haxe Foundation
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

// This file is generated from mozilla/AudioContext.webidl line 18:0. Do not edit!

package js.html.audio;

@:native("AudioContext")
extern class AudioContext extends js.html.EventTarget
{
	var destination(default,null) : AudioDestinationNode;
	var sampleRate(default,null) : Float;
	var currentTime(default,null) : Float;
	var listener(default,null) : AudioListener;
	
	/** @throws DOMError */
	@:overload( function() : Void {} )
	function new( audioChannelType : js.html.AudioChannel ) : Void;
	/** @throws DOMError */
	function createBuffer( numberOfChannels : Int, length : Int, sampleRate : Float ) : AudioBuffer;
	function decodeAudioData( audioData : js.html.ArrayBuffer, ?successCallback : AudioBuffer -> Void, ?errorCallback : Void -> Void ) : Promise<AudioBuffer>;
	function createBufferSource() : AudioBufferSourceNode;
	/** @throws DOMError */
	function createMediaStreamDestination() : MediaStreamAudioDestinationNode;
	/** @throws DOMError */
	function createScriptProcessor( ?bufferSize : Int = 0, ?numberOfInputChannels : Int = 2, ?numberOfOutputChannels : Int = 2 ) : ScriptProcessorNode;
	function createStereoPanner() : StereoPannerNode;
	function createAnalyser() : AnalyserNode;
	/** @throws DOMError */
	function createMediaElementSource( mediaElement : js.html.MediaElement ) : MediaElementAudioSourceNode;
	/** @throws DOMError */
	function createMediaStreamSource( mediaStream : js.html.MediaStream ) : MediaStreamAudioSourceNode;
	function createGain() : GainNode;
	/** @throws DOMError */
	function createDelay( ?maxDelayTime : Float = 1.0 ) : DelayNode;
	function createBiquadFilter() : BiquadFilterNode;
	function createWaveShaper() : WaveShaperNode;
	function createPanner() : PannerNode;
	function createConvolver() : ConvolverNode;
	/** @throws DOMError */
	function createChannelSplitter( ?numberOfOutputs : Int = 6 ) : ChannelSplitterNode;
	/** @throws DOMError */
	function createChannelMerger( ?numberOfInputs : Int = 6 ) : ChannelMergerNode;
	function createDynamicsCompressor() : DynamicsCompressorNode;
	function createOscillator() : OscillatorNode;
	/** @throws DOMError */
	function createPeriodicWave( real : js.html.Float32Array, imag : js.html.Float32Array ) : PeriodicWave;
}