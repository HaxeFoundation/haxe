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

/** <p>XSLTProcesor is an object providing an interface to XSLT engine in Mozilla. It is available to unprivileged JavaScript.</p>
<ul> <li><a title="en/Using_the_Mozilla_JavaScript_interface_to_XSL_Transformations" rel="internal" href="https://developer.mozilla.org/en/Using_the_Mozilla_JavaScript_interface_to_XSL_Transformations">Using the Mozilla JavaScript interface to XSL Transformations</a></li> <li><a title="en/The_XSLT//JavaScript_Interface_in_Gecko" rel="internal" href="https://developer.mozilla.org/en/The_XSLT%2F%2FJavaScript_Interface_in_Gecko">The XSLT/JavaScript Interface in Gecko</a></li>
</ul><br><br>
Documentation for this class was provided by <a href="https://developer.mozilla.org/en/XSLTProcessor">MDN</a>. */
@:native("XSLTProcessor")
extern class XSLTProcessor
{
	function new() : Void;

	function clearParameters() : Void;

	function getParameter( namespaceURI : String, localName : String ) : String;

	function importStylesheet( stylesheet : Node ) : Void;

	function removeParameter( namespaceURI : String, localName : String ) : Void;

	function reset() : Void;

	function setParameter( namespaceURI : String, localName : String, value : String ) : Void;

	function transformToDocument( source : Node ) : Document;

	function transformToFragment( source : Node, docVal : Document ) : DocumentFragment;

}
