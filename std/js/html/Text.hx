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

/** <p>In the <a title="en/DOM" rel="internal" href="https://developer.mozilla.org/en/DOM">DOM</a>, the Text interface represents the textual content of an <a class="internal" title="En/DOM/Element" rel="internal" href="/api/js/html/Element">Element</a> or <a class="internal" title="En/DOM/Attr" rel="internal" href="https://developer.mozilla.org/En/DOM/Attr">Attr</a>.&nbsp; If an element has no markup within its content, it has a single child implementing Text that contains the element's text.&nbsp; However, if the element contains markup, it is parsed into information items and Text nodes that form its children.</p>
<p>New documents have a single Text node for each block of text.&nbsp; Over time, more Text nodes may be created as the document's content changes.&nbsp; The <code>Node.normalize()</code>&nbsp;method merges adjacent Text objects back into a single node for each block of text.</p>
<p>Text also implements the <a title="En/DOM/CharacterData" rel="internal" href="/api/js/html/CharacterData">CharacterData</a> interface (which implements the Node interface).</p><br><br>
Documentation for this class was provided by <a href="https://developer.mozilla.org/en/DOM/Text">MDN</a>. */
@:native("Text")
extern class Text extends CharacterData
{
	/** Returns all text of all Text nodes logically adjacent to this node, concatenated in document order. */
	var wholeText(default,null) : String;

	function replaceWholeText( content : String ) : Text;

	function splitText( offset : Int ) : Text;

}
