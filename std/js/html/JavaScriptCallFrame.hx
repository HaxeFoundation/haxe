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

@:native("JavaScriptCallFrame")
extern class JavaScriptCallFrame
{
	static inline var CATCH_SCOPE : Int = 4;

	static inline var CLOSURE_SCOPE : Int = 3;

	static inline var GLOBAL_SCOPE : Int = 0;

	static inline var LOCAL_SCOPE : Int = 1;

	static inline var WITH_SCOPE : Int = 2;

	var caller (default,null) : JavaScriptCallFrame;

	var column (default,null) : Int;

	var functionName (default,null) : String;

	var line (default,null) : Int;

	var scopeChain (default,null) : Array<Dynamic>;

	var sourceID (default,null) : Int;

	var thisObject (default,null) : Dynamic;

	var type (default,null) : String;

	function evaluate( script : String ) : Void;

	function restart() : Dynamic;

	function scopeType( scopeIndex : Int ) : Int;

}
