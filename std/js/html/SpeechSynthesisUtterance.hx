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

// This file is generated from mozilla\SpeechSynthesisUtterance.webidl. Do not edit!

package js.html;

/**
	The `SpeechSynthesisUtterance` interface of the Web Speech API represents a speech request. It contains the content the speech service should read and information about how to read it (e.g. language, pitch and volume.)

	Documentation [SpeechSynthesisUtterance](https://developer.mozilla.org/en-US/docs/Web/API/SpeechSynthesisUtterance) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/SpeechSynthesisUtterance$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/SpeechSynthesisUtterance>
**/
@:native("SpeechSynthesisUtterance")
extern class SpeechSynthesisUtterance extends EventTarget
{
	
	/**
		Gets and sets the text that will be synthesised when the utterance is spoken.
	**/
	var text : String;
	
	/**
		Gets and sets the language of the utterance.
	**/
	var lang : String;
	
	/**
		Gets and sets the voice that will be used to speak the utterance.
	**/
	var voice : SpeechSynthesisVoice;
	
	/**
		Gets and sets the volume that the utterance will be spoken at.
	**/
	var volume : Float;
	
	/**
		Gets and sets the speed at which the utterance will be spoken at.
	**/
	var rate : Float;
	
	/**
		Gets and sets the pitch at which the utterance will be spoken at.
	**/
	var pitch : Float;
	
	/**
		Fired when the utterance has begun to be spoken.
	**/
	var onstart : haxe.Constraints.Function;
	
	/**
		Fired when the utterance has finished being spoken.
	**/
	var onend : haxe.Constraints.Function;
	
	/**
		Fired when an error occurs that prevents the utterance from being succesfully spoken.
	**/
	var onerror : haxe.Constraints.Function;
	
	/**
		Fired when the utterance is paused part way through.
	**/
	var onpause : haxe.Constraints.Function;
	
	/**
		Fired when a paused utterance is resumed.
	**/
	var onresume : haxe.Constraints.Function;
	
	/**
		Fired when the spoken utterance reaches a named SSML "mark" tag.
	**/
	var onmark : haxe.Constraints.Function;
	
	/**
		Fired when the spoken utterance reaches a word or sentence boundary.
	**/
	var onboundary : haxe.Constraints.Function;
	
	/** @throws DOMError */
	@:overload( function() : Void {} )
	function new( text : String ) : Void;
}