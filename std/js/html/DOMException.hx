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

// This file is generated from mozilla\DOMException.webidl. Do not edit!

package js.html;

/**
	The `DOMException` interface represents an abnormal event (called an exception) which occurs as a result of calling a method or accessing a property of a web API. This is basically how error conditions are described in web APIs.

	Documentation [DOMException](https://developer.mozilla.org/en-US/docs/Web/API/DOMException) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/DOMException$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/DOMException>
**/
@:native("DOMException")
extern class DOMException
{
	static inline var INDEX_SIZE_ERR : Int = 1;
	static inline var DOMSTRING_SIZE_ERR : Int = 2;
	static inline var HIERARCHY_REQUEST_ERR : Int = 3;
	static inline var WRONG_DOCUMENT_ERR : Int = 4;
	static inline var INVALID_CHARACTER_ERR : Int = 5;
	static inline var NO_DATA_ALLOWED_ERR : Int = 6;
	static inline var NO_MODIFICATION_ALLOWED_ERR : Int = 7;
	static inline var NOT_FOUND_ERR : Int = 8;
	static inline var NOT_SUPPORTED_ERR : Int = 9;
	static inline var INUSE_ATTRIBUTE_ERR : Int = 10;
	static inline var INVALID_STATE_ERR : Int = 11;
	static inline var SYNTAX_ERR : Int = 12;
	static inline var INVALID_MODIFICATION_ERR : Int = 13;
	static inline var NAMESPACE_ERR : Int = 14;
	static inline var INVALID_ACCESS_ERR : Int = 15;
	static inline var VALIDATION_ERR : Int = 16;
	static inline var TYPE_MISMATCH_ERR : Int = 17;
	static inline var SECURITY_ERR : Int = 18;
	static inline var NETWORK_ERR : Int = 19;
	static inline var ABORT_ERR : Int = 20;
	static inline var URL_MISMATCH_ERR : Int = 21;
	static inline var QUOTA_EXCEEDED_ERR : Int = 22;
	static inline var TIMEOUT_ERR : Int = 23;
	static inline var INVALID_NODE_TYPE_ERR : Int = 24;
	static inline var DATA_CLONE_ERR : Int = 25;
	
	var code(default,null) : Int;
	var message(default,null) : String;
	var result(default,null) : Int;
	var name(default,null) : String;
	var filename(default,null) : String;
	var lineNumber(default,null) : Int;
	var columnNumber(default,null) : Int;
	var data(default,null) : Dynamic/*MISSING nsISupports*/;
	var stack(default,null) : String;
	
	/** @throws DOMError */
	function new( ?message : String = "", ?name : String ) : Void;
}