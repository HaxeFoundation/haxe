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

// This file is generated from mozilla\SpeechGrammarList.webidl. Do not edit!

package js.html;

/**
	The `SpeechGrammarList` interface of the Web Speech API represents a list of `SpeechGrammar` objects containing words or patterns of words that we want the recognition service to recognize.

	Documentation [SpeechGrammarList](https://developer.mozilla.org/en-US/docs/Web/API/SpeechGrammarList) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/SpeechGrammarList$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/SpeechGrammarList>
**/
@:native("SpeechGrammarList")
extern class SpeechGrammarList implements ArrayAccess<SpeechGrammar>
{
	
	/**
		Returns the number of `SpeechGrammar` objects contained in the `SpeechGrammarList`.
	**/
	var length(default,null) : Int;
	
	/** @throws DOMError */
	function new() : Void;
	/** @throws DOMError */
	
	/**
		Standard getter — allows individual `SpeechGrammar` objects to be retrieved from the `SpeechGrammarList` using array syntax.
	**/
	function item( index : Int ) : SpeechGrammar;
	/** @throws DOMError */
	
	/**
		Takes a grammar present at a specific URI and adds it to the `SpeechGrammarList` as a new `SpeechGrammar` object.
	**/
	function addFromURI( src : String, ?weight : Float ) : Void;
	/** @throws DOMError */
	
	/**
		Takes a grammar present in a specific `DOMString` within the code base (e.g. stored in a variable) and adds it to the `SpeechGrammarList` as a new `SpeechGrammar` object.
	**/
	function addFromString( string : String, ?weight : Float ) : Void;
}