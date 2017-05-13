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

// This file is generated from mozilla\SpeechSynthesis.webidl. Do not edit!

package js.html;

/**
	The `SpeechSynthesis` interface of the Web Speech API is the controller interface for the speech service; this can be used to retrieve information about the synthesis voices available on the device, start and pause speech, and other commands besides.

	Documentation [SpeechSynthesis](https://developer.mozilla.org/en-US/docs/Web/API/SpeechSynthesis) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/SpeechSynthesis$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/SpeechSynthesis>
**/
@:native("SpeechSynthesis")
extern class SpeechSynthesis
{
	
	/**
		A `Boolean` that returns `true` if the utterance queue contains as-yet-unspoken utterances.
	**/
	var pending(default,null) : Bool;
	
	/**
		A `Boolean` that returns `true` if an utterance is currently in the process of being spoken — even if `SpeechSynthesis` is in a paused state.
	**/
	var speaking(default,null) : Bool;
	
	/**
		A `Boolean` that returns `true` if the `SpeechSynthesis` object is in a paused state.
	**/
	var paused(default,null) : Bool;
	
	
	/**
		Adds an `SpeechSynthesisUtterance` to the utterance queue; it will be spoken when any other utterances queued before it have been spoken.
	**/
	function speak( utterance : SpeechSynthesisUtterance ) : Void;
	
	/**
		Removes all utterances from the utterance queue.
	**/
	function cancel() : Void;
	
	/**
		Puts the `SpeechSynthesis` object into a paused state.
	**/
	function pause() : Void;
	
	/**
		Puts the `SpeechSynthesis` object into a non-paused state: resumes it if it was already paused.
	**/
	function resume() : Void;
	
	/**
		Returns a list of `SpeechSynthesisVoice` objects representing all the available voices on the current device.
	**/
	function getVoices() : Array<SpeechSynthesisVoice>;
}