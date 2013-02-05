/*
 * Copyright (C)2005-2013 Haxe Foundation
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

// This file is generated, do not edit!
package js.html.audio;

@:native("AudioContext")
extern class AudioContext extends js.html.EventTarget
{
	var activeSourceCount(default,null) : Int;

	var currentTime(default,null) : Float;

	var destination(default,null) : AudioDestinationNode;

	var listener(default,null) : AudioListener;

	var oncomplete : js.html.EventListener;

	var sampleRate(default,null) : Float;

	function new() : Void;

	function createAnalyser() : AnalyserNode;

	function createBiquadFilter() : BiquadFilterNode;

	/** Throws DOMException. */
	@:overload( function( numberOfChannels : Int, numberOfFrames : Int, sampleRate : Float ) :AudioBuffer {} )
	function createBuffer( buffer : js.html.ArrayBuffer, mixToMono : Bool ) : AudioBuffer;

	function createBufferSource() : AudioBufferSourceNode;

	function createChannelMerger( ?numberOfInputs : Int ) : ChannelMergerNode;

	function createChannelSplitter( ?numberOfOutputs : Int ) : ChannelSplitterNode;

	function createConvolver() : ConvolverNode;

	function createDelay( ?maxDelayTime : Float ) : DelayNode;

	function createDynamicsCompressor() : DynamicsCompressorNode;

	function createGain() : GainNode;

	function createMediaElementSource( mediaElement : js.html.MediaElement ) : MediaElementAudioSourceNode;

	function createMediaStreamSource( mediaStream : js.html.rtc.MediaStream ) : MediaStreamAudioSourceNode;

	function createOscillator() : OscillatorNode;

	function createPanner() : PannerNode;

	function createScriptProcessor( bufferSize : Int, ?numberOfInputChannels : Int, ?numberOfOutputChannels : Int ) : ScriptProcessorNode;

	function createWaveShaper() : WaveShaperNode;

	function createWaveTable( real : js.html.Float32Array, imag : js.html.Float32Array ) : WaveTable;

	function decodeAudioData( audioData : js.html.ArrayBuffer, successCallback : AudioBufferCallback, ?errorCallback : AudioBufferCallback ) : Void;

	function startRendering() : Void;

}
