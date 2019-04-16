/*
 * Copyright (C)2005-2019 Haxe Foundation
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

// This file is generated from mozilla\CharacterData.webidl. Do not edit!

package js.html;

/**
	The `CharacterData` abstract interface represents a `Node` object that contains characters. This is an abstract interface, meaning there aren't any object of type `CharacterData`: it is implemented by other interfaces, like `Text`, '. In XML, the character sequence '--' cannot be used within a comment.">`Comment`, or `ProcessingInstruction` which aren't abstract.

	Documentation [CharacterData](https://developer.mozilla.org/en-US/docs/Web/API/CharacterData) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/CharacterData$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/CharacterData>
**/
@:native("CharacterData")
extern class CharacterData extends Node {
	
	/**
		Is a `DOMString` representing the textual data contained in this object.
	**/
	var data : String;
	
	/**
		Returns an `unsigned long` representing the size of the string contained in `CharacterData.data`.
	**/
	var length(default,null) : Int;
	var previousElementSibling(default,null) : Element;
	var nextElementSibling(default,null) : Element;
	
	
	/**
		Returns a `DOMString` containing the part of `CharacterData.data` of the specified length and starting at the specified offset.
		@throws DOMError
	**/
	function substringData( offset : Int, count : Int ) : String;
	
	/**
		Appends the given `DOMString` to the `CharacterData.data` string; when this method returns, `data` contains the concatenated `DOMString`.
		@throws DOMError
	**/
	function appendData( data : String ) : Void;
	
	/**
		Inserts the specified characters, at the specified offset, in the `CharacterData.data` string; when this method returns, `data` contains the modified `DOMString`.
		@throws DOMError
	**/
	function insertData( offset : Int, data : String ) : Void;
	
	/**
		Removes the specified amount of characters, starting at the specified offset, from the `CharacterData.data` string; when this method returns, `data` contains the shortened `DOMString`.
		@throws DOMError
	**/
	function deleteData( offset : Int, count : Int ) : Void;
	
	/**
		Replaces the specified amount of characters, starting at the specified offset, with the specified `DOMString`; when this method returns, `data` contains the modified `DOMString`.
		@throws DOMError
	**/
	function replaceData( offset : Int, count : Int, data : String ) : Void;
	/** @throws DOMError */
	@:overload( function( nodes : haxe.extern.Rest<String>) : Void {} )
	function before( nodes : haxe.extern.Rest<Node> ) : Void;
	/** @throws DOMError */
	@:overload( function( nodes : haxe.extern.Rest<String>) : Void {} )
	function after( nodes : haxe.extern.Rest<Node> ) : Void;
	/** @throws DOMError */
	@:overload( function( nodes : haxe.extern.Rest<String>) : Void {} )
	function replaceWith( nodes : haxe.extern.Rest<Node> ) : Void;
	function remove() : Void;
}