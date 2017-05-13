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

// This file is generated from mozilla\SpeechSynthesisEvent.webidl. Do not edit!

package js.html;

/**
	The `SpeechSynthesisEvent` interface of the Web Speech API contains information about the current state of `SpeechSynthesisUtterance` objects that have been processed in the speech service.

	Documentation [SpeechSynthesisEvent](https://developer.mozilla.org/en-US/docs/Web/API/SpeechSynthesisEvent) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/SpeechSynthesisEvent$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/SpeechSynthesisEvent>
**/
@:native("SpeechSynthesisEvent")
extern class SpeechSynthesisEvent extends Event
{
	
	/**
		Returns the `SpeechSynthesisUtterance` instance that the event was triggered on.
	**/
	var utterance(default,null) : SpeechSynthesisUtterance;
	
	/**
		Returns the index position of the character in the `SpeechSynthesisUtterance.text` that was being spoken when the event was triggered.
	**/
	var charIndex(default,null) : Int;
	
	/**
		Returns the elapsed time in milliseconds after the `SpeechSynthesisUtterance.text` started being spoken that the event was triggered at.
	**/
	var elapsedTime(default,null) : Float;
	
	/**
		Returns the name associated with certain types of events occurring as the `SpeechSynthesisUtterance.text` is being spoken: the name of the SSML marker reached in the case of a `mark` event, or the type of boundary reached in the case of a `boundary` event.
	**/
	var name(default,null) : String;
	
	/** @throws DOMError */
	function new( type : String, eventInitDict : SpeechSynthesisEventInit ) : Void;
}