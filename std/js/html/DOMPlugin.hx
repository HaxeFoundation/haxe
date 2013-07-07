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

/** The <code>Plugin</code> interface provides information about a browser plugin.<br><br>
Documentation for this class was provided by <a href="https://developer.mozilla.org/en/DOM/Plugin">MDN</a>. */
@:native("Plugin")
extern class DOMPlugin implements ArrayAccess<DOMMimeType>
{
	/** A human readable description of the plugin. <strong>Read only.</strong> */
	var description(default,null) : String;

	/** The filename of the plugin file. <strong>Read only.</strong> */
	var filename(default,null) : String;

	var length(default,null) : Int;

	/** The name of the plugin. <strong>Read only.</strong> */
	var name(default,null) : String;

	function item( index : Int ) : DOMMimeType;

	function namedItem( name : String ) : DOMMimeType;

}
