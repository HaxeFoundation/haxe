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

// This file is generated from mozilla\XPathEvaluator.webidl. Do not edit!

package js.html;

@:native("XPathEvaluator")
extern class XPathEvaluator {
	/** @throws DOMError */
	function new() : Void;
	/** @throws DOMError */
	@:overload( function( expression : String, ?resolver : haxe.Constraints.Function) : XPathExpression {} )
	@:overload( function( expression : String, ?resolver : XPathNSResolver) : XPathExpression {} )
	function createExpression( expression : String, ?resolver : String -> Null<String> ) : XPathExpression;
	@:pure
	function createNSResolver( nodeResolver : Node ) : Node;
	/** @throws DOMError */
	@:overload( function( expression : String, contextNode : Node, ?resolver : haxe.Constraints.Function, type : Int = 0, ?result : Dynamic) : XPathResult {} )
	@:overload( function( expression : String, contextNode : Node, ?resolver : XPathNSResolver, type : Int = 0, ?result : Dynamic) : XPathResult {} )
	function evaluate( expression : String, contextNode : Node, ?resolver : String -> Null<String>, type : Int = 0, ?result : Dynamic ) : XPathResult;
}