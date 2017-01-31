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

// This file is generated from mozilla\SpeechRecognitionEvent.webidl. Do not edit!

package js.html;

/**
	The `SpeechRecognitionEvent` interface of the Web Speech API represents the event object for the `result` and `nomatch` events, and contains all the data associated with an interim or final speech recognition result.

	Documentation [SpeechRecognitionEvent](https://developer.mozilla.org/en-US/docs/Web/API/SpeechRecognitionEvent) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/SpeechRecognitionEvent$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/SpeechRecognitionEvent>
**/
@:native("SpeechRecognitionEvent")
extern class SpeechRecognitionEvent extends Event
{
	
	/**
		Returns the lowest index value result in the `SpeechRecognitionResultList` "array" that has actually changed.
	**/
	var resultIndex(default,null) : Int;
	
	/**
		Returns a `SpeechRecognitionResultList` object representing all the speech recognition results for the current session.
	**/
	var results(default,null) : SpeechRecognitionResultList;
	
	/**
		Returns the semantic meaning of what the user said.
	**/
	var interpretation(default,null) : Dynamic;
	
	/**
		Returns an Extensible MultiModal Annotation markup language (EMMA) — XML — representation of the result.
	**/
	var emma(default,null) : HTMLDocument;
	
	/** @throws DOMError */
	function new( type : String, ?eventInitDict : SpeechRecognitionEventInit ) : Void;
}