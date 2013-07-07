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
package js.html.svg;

/** <p>The <code>SVGStringList</code> defines a list of <code><a rel="custom" href="https://developer.mozilla.org/en/DOM/DOMString">DOMString</a></code>
 objects.</p>
<p>An <code>SVGStringList</code> object can be designated as read only, which means that attempts to modify the object will result in an exception being thrown.</p><br><br>
Documentation for this class was provided by <a href="https://developer.mozilla.org/en/DOM/SVGStringList">MDN</a>. */
@:native("SVGStringList")
extern class StringList
{
	var numberOfItems(default,null) : Int;

	function appendItem( item : String ) : String;

	function clear() : Void;

	function getItem( index : Int ) : String;

	function initialize( item : String ) : String;

	function insertItemBefore( item : String, index : Int ) : String;

	function removeItem( index : Int ) : String;

	function replaceItem( item : String, index : Int ) : String;

}
