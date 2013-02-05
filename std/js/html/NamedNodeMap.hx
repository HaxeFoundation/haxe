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

/** A collection of nodes returned by <a title="En/DOM/Element.attributes" class="internal" rel="internal" href="https://developer.mozilla.org/En/DOM/Node.attributes"><code>Element.attributes</code></a> (also potentially for <code><a title="En/DOM/DocumentType.entities" rel="internal" href="https://developer.mozilla.org/En/DOM/DocumentType.entities" class="new internal">DocumentType.entities</a></code>, <code><a title="En/DOM/DocumentType.notations" rel="internal" href="https://developer.mozilla.org/En/DOM/DocumentType.notations" class="new internal">DocumentType.notations</a></code>). <code>NamedNodeMap</code>s are not in any particular order (unlike <code><a title="En/DOM/NodeList" class="internal" rel="internal" href="https://developer.mozilla.org/En/DOM/NodeList">NodeList</a></code>), although they may be accessed by an index as in an array (they may also be accessed with the <code>item</code>() method). A NamedNodeMap object are live and will thus be auto-updated if changes are made to their contents internally or elsewhere.<br><br>
Documentation for this class was provided by <a href="https://developer.mozilla.org/en/DOM/NamedNodeMap">MDN</a>. */
@:native("NamedNodeMap")
extern class NamedNodeMap implements ArrayAccess<Node>
{
	var length(default,null) : Int;

	function getNamedItem( name : String ) : Node;

	function getNamedItemNS( namespaceURI : String, localName : String ) : Node;

	function item( index : Int ) : Node;

	function removeNamedItem( name : String ) : Node;

	function removeNamedItemNS( namespaceURI : String, localName : String ) : Node;

	function setNamedItem( node : Node ) : Node;

	function setNamedItemNS( node : Node ) : Node;

}
