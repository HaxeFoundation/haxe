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
package js.html;

@:native("SpeechRecognitionError")
extern class SpeechRecognitionError extends Event
{
	static inline var ABORTED : Int = 2;

	static inline var AUDIO_CAPTURE : Int = 3;

	static inline var BAD_GRAMMAR : Int = 7;

	static inline var LANGUAGE_NOT_SUPPORTED : Int = 8;

	static inline var NETWORK : Int = 4;

	static inline var NOT_ALLOWED : Int = 5;

	static inline var NO_SPEECH : Int = 1;

	static inline var OTHER : Int = 0;

	static inline var SERVICE_NOT_ALLOWED : Int = 6;

	var code(default,null) : Int;

	var message(default,null) : String;

	function new(type : String, canBubble : Bool = true, cancelable : Bool = true) : Void;

}
