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

// This file is generated from mozilla\SpeechRecognitionAlternative.webidl. Do not edit!

package js.html;

/**
	The `SpeechRecognitionAlternative` interface of the Web Speech API represents a single word that has been recognised by the speech recognition service.

	Documentation [SpeechRecognitionAlternative](https://developer.mozilla.org/en-US/docs/Web/API/SpeechRecognitionAlternative) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/SpeechRecognitionAlternative$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/SpeechRecognitionAlternative>
**/
@:native("SpeechRecognitionAlternative")
extern class SpeechRecognitionAlternative
{
	
	/**
		Returns a string containing the transcript of the recognised word.
	**/
	var transcript(default,null) : String;
	
	/**
		Returns a numeric estimate of how confident the speech recognition system is that the recognition is correct.
	**/
	var confidence(default,null) : Float;
	
}