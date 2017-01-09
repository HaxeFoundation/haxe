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

// This file is generated from mozilla\SpeechRecognition.webidl. Do not edit!

package js.html;

/**
	The `SpeechRecognition` interface of the Web Speech API is the controller interface for the recognition service; this also handles the `SpeechRecognitionEvent` sent from the recognition service.

	Documentation [SpeechRecognition](https://developer.mozilla.org/en-US/docs/Web/API/SpeechRecognition) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/SpeechRecognition$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/SpeechRecognition>
**/
@:native("SpeechRecognition")
extern class SpeechRecognition extends EventTarget
{
	
	/**
		Returns and sets a collection of `SpeechGrammar` objects that represent the grammars that will be understood by the current `SpeechRecognition`.
	**/
	var grammars : SpeechGrammarList;
	
	/**
		Returns and sets the language of the current `SpeechRecognition`. If not specified, this defaults to the HTML `lang` attribute value, or the user agent's language setting if that isn't set either.
	**/
	var lang : String;
	
	/**
		Controls whether continuous results are returned for each recognition, or only a single result. Defaults to single (`false`.)
	**/
	var continuous : Bool;
	
	/**
		Controls whether interim results should be returned (`true`) or not (`false`.) Interim results are results that are not yet final (e.g. the `SpeechRecognitionResult.isFinal` property is `false`.)
	**/
	var interimResults : Bool;
	
	/**
		Sets the maximum number of `SpeechRecognitionAlternative`s provided per result. The default value is 1.
	**/
	var maxAlternatives : Int;
	
	/**
		Specifies the location of the speech recognition service used by the current `SpeechRecognition` to handle the actual recognition. The default is the user agent's default speech service.
	**/
	var serviceURI : String;
	
	/**
		Fired when the user agent has started to capture audio.
	**/
	var onaudiostart : haxe.Constraints.Function;
	
	/**
		Fired when any sound — recognisable speech or not — has been detected.
	**/
	var onsoundstart : haxe.Constraints.Function;
	
	/**
		Fired when sound that is recognised by the speech recognition service as speech has been detected.
	**/
	var onspeechstart : haxe.Constraints.Function;
	
	/**
		Fired when speech recognised by the speech recognition service has stopped being detected.
	**/
	var onspeechend : haxe.Constraints.Function;
	
	/**
		Fired when any sound — recognisable speech or not — has stopped being detected.
	**/
	var onsoundend : haxe.Constraints.Function;
	
	/**
		Fired when the user agent has finished capturing audio.
	**/
	var onaudioend : haxe.Constraints.Function;
	
	/**
		Fired when the speech recognition service returns a result — a word or phrase has been positively recognized and this has been communicated back to the app.
	**/
	var onresult : haxe.Constraints.Function;
	
	/**
		Fired when the speech recognition service returns a final result with no significant recognition. This may involve some degree of recognition, which doesn't meet or exceed the `SpeechRecognitionAlternative.confidence` threshold.
	**/
	var onnomatch : haxe.Constraints.Function;
	
	/**
		Fired when a speech recognition error occurs.
	**/
	var onerror : haxe.Constraints.Function;
	
	/**
		Fired when the speech recognition service has begun listening to incoming audio with intent to recognize grammars associated with the current `SpeechRecognition`.
	**/
	var onstart : haxe.Constraints.Function;
	
	/**
		Fired when the speech recognition service has disconnected.
	**/
	var onend : haxe.Constraints.Function;
	
	/** @throws DOMError */
	function new() : Void;
	/** @throws DOMError */
	
	/**
		Starts the speech recognition service listening to incoming audio with intent to recognize grammars associated with the current `SpeechRecognition`.
	**/
	function start( ?stream : MediaStream ) : Void;
	
	/**
		Stops the speech recognition service from listening to incoming audio, and attempts to return a `SpeechRecognitionResult` using the audio captured so far.
	**/
	function stop() : Void;
	
	/**
		Stops the speech recognition service from listening to incoming audio, and doesn't attempt to return a `SpeechRecognitionResult`.
	**/
	function abort() : Void;
}