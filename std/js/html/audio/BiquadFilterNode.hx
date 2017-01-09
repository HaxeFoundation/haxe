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

// This file is generated from mozilla\BiquadFilterNode.webidl. Do not edit!

package js.html.audio;

/**
	The `BiquadFilterNode` interface represents a simple low-order filter, and is created using the `AudioContext.createBiquadFilter()` method. It is an `AudioNode` that can represent different kinds of filters, tone control devices, and graphic equalizers.

	Documentation [BiquadFilterNode](https://developer.mozilla.org/en-US/docs/Web/API/BiquadFilterNode) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/BiquadFilterNode$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/BiquadFilterNode>
**/
@:native("BiquadFilterNode")
extern class BiquadFilterNode extends AudioNode
{
	
	/**
		Is a string value defining the kind of filtering algorithm the node is implementing.<br>
		  
		 <table class="standard-table">
		  The meaning of the different parameters depending of the type of the filter (detune has the same meaning regardless, so isn't listed below)
		  
		   <tr>
		    <code>type</code>
		    Description
		    <code>frequency</code>
		    <code>Q</code>
		    <code>gain</code>
		   </tr>
		  
		  
		   <tr>
		    <code>lowpass</code>
		    <td>Standard second-order resonant lowpass filter with 12dB/octave rolloff. Frequencies below the cutoff pass through; frequencies above it are attenuated.</td>
		    <td>The cutoff frequency.</td>
		    <td>Indicates how peaked the frequency is around the cutoff. The greater the value is, the greater is the peak.</td>
		    <td>Not used</td>
		   </tr>
		   <tr>
		    <code>highpass</code>
		    <td>Standard second-order resonant highpass filter with 12dB/octave rolloff. Frequencies below the cutoff are attenuated; frequencies above it pass through.</td>
		    <td>The cutoff frequency.</td>
		    <td>Indicates how peaked the frequency is around the cutoff. The greater the value, the greater the peak.</td>
		    <td>Not used</td>
		   </tr>
		   <tr>
		    <code>bandpass</code>
		    <td>Standard second-order bandpass filter. Frequencies outside the given range of frequencies are attenuated; the frequencies inside it pass through.</td>
		    <td>The center of the range of frequencies.</td>
		    <td>Controls the width of the frequency band. The greater the <code>Q</code> value, the smaller the frequency band.</td>
		    <td>Not used</td>
		   </tr>
		   <tr>
		    <code>lowshelf</code>
		    <td>Standard second-order lowshelf filer. Frequencies lower than the frequency get a boost, or an attenuation; frequencies over it are unchanged.</td>
		    <td>The upper limit of the frequencies getting a boost or an attenuation.</td>
		    <td>Not used</td>
		    <td>The boost, in dB, to be applied; if negative, it will be an attenuation.</td>
		   </tr>
		   <tr>
		    <code>highshelf</code>
		    <td>Standard second-order highshelf filer. Frequencies higher than the frequency get a boost or an attenuation; frequencies lower than it are unchanged.</td>
		    <td>The lower limit of the frequencies getting a boost or an attenuation.</td>
		    <td>Not used</td>
		    <td>The boost, in dB, to be applied; if negative, it will be an attenuation.</td>
		   </tr>
		   <tr>
		    <code>peaking</code>
		    <td>Frequencies inside the range get a boost or an attenuation; frequencies outside it are unchanged.</td>
		    <td>The middle of the frequency range getting a boost or an attenuation.</td>
		    <td>Controls the width of the frequency band. The greater the <code>Q</code> value, the smaller the frequency band.</td>
		    <td>The boost, in dB, to be applied; if negative, it will be an attenuation.</td>
		   </tr>
		   <tr>
		    <code>notch</code>
		    <td>Standard notch filter, also called a band-stop or band-rejection filter. It is the opposite of a bandpass filter: frequencies outside the give range of frequencies pass through; frequencies inside it are attenuated.</td>
		    <td>The center of the range of frequencies.</td>
		    <td>Controls the width of the frequency band. The greater the <code>Q</code> value, the smaller the frequency band.</td>
		    <td>Not used</td>
		   </tr>
		   <tr>
		    <code>allpass</code>
		    <td>Standard second-order allpass filter. It lets all frequencies through, but changes the phase-relationship between the various frequencies.</td>
		    <td>The frequency with the maximal group delay, that is, the frequency where the center of the phase transition occurs.</td>
		    <td>Controls how sharp the transition is at the medium frequency. The larger this parameter is, the sharper and larger the transition will be.</td>
		    <td>Not used</td>
		   </tr>
		  
		 </table>
		 
	**/
	var type : BiquadFilterType;
	
	/**
		Is an a-rate `AudioParam`, a double representing a frequency in the current filtering algorithm measured in hertz (Hz).
	**/
	var frequency(default,null) : AudioParam;
	
	/**
		Is an a-rate `AudioParam` representing detuning of the frequency in cents.
	**/
	var detune(default,null) : AudioParam;
	
	/**
		Is an a-rate `AudioParam`, a double representing a Q factor, or quality factor.
	**/
	var Q(default,null) : AudioParam;
	
	/**
		Is an a-rate `AudioParam`, a double representing the gain used in the current filtering algorithm.
	**/
	var gain(default,null) : AudioParam;
	
	
	/**
		From the current filter parameter settings this method calculates the frequency response for frequencies specified in the provided array of frequencies.
	**/
	function getFrequencyResponse( frequencyHz : js.html.Float32Array, magResponse : js.html.Float32Array, phaseResponse : js.html.Float32Array ) : Void;
}