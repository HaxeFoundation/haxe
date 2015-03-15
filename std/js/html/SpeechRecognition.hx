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

// This file is generated from mozilla/SpeechRecognition.webidl line 15:0. Do not edit!

package js.html;

@:native("SpeechRecognition")
extern class SpeechRecognition extends EventTarget
{
	var grammars : SpeechGrammarList;
	var lang : String;
	var continuous : Bool;
	var interimResults : Bool;
	var maxAlternatives : Int;
	var serviceURI : String;
	var onaudiostart : haxe.Constraints.Function;
	var onsoundstart : haxe.Constraints.Function;
	var onspeechstart : haxe.Constraints.Function;
	var onspeechend : haxe.Constraints.Function;
	var onsoundend : haxe.Constraints.Function;
	var onaudioend : haxe.Constraints.Function;
	var onresult : haxe.Constraints.Function;
	var onnomatch : haxe.Constraints.Function;
	var onerror : haxe.Constraints.Function;
	var onstart : haxe.Constraints.Function;
	var onend : haxe.Constraints.Function;
	
	/** @throws DOMError */
	function new() : Void;
	/** @throws DOMError */
	function start( ?stream : MediaStream ) : Void;
	function stop() : Void;
	function abort() : Void;
}