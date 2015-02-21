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

// This file is generated from mozilla/SpeechSynthesisUtterance.webidl line 16:0. Do not edit!

package js.html;

@:native("SpeechSynthesisUtterance")
extern class SpeechSynthesisUtterance extends EventTarget
{
	var text : String;
	var lang : String;
	var voice : SpeechSynthesisVoice;
	var volume : Float;
	var rate : Float;
	var pitch : Float;
	var onstart : haxe.Constraints.Function;
	var onend : haxe.Constraints.Function;
	var onerror : haxe.Constraints.Function;
	var onpause : haxe.Constraints.Function;
	var onresume : haxe.Constraints.Function;
	var onmark : haxe.Constraints.Function;
	var onboundary : haxe.Constraints.Function;
	
	/** @throws DOMError */
	@:overload( function() : Void {} )
	function new( text : String ) : Void;
}