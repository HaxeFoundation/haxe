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

/** <p>A processing instruction provides an opportunity for application-specific instructions to be embedded within XML and which can be ignored by XML processors which do not support processing their instructions (outside of their having a place in the DOM).</p>
<p>A Processing instruction is distinct from a <a title="en/XML/XML_Declaration" rel="internal" href="https://developer.mozilla.org/en/XML/XML_Declaration" class="new ">XML Declaration</a> which is used for other information about the document such as encoding and which appear (if it does) as the first item in the document.</p>
<p>User-defined processing instructions cannot begin with 'xml', as these are reserved (e.g., as used in &lt;?<a title="en/XML/xml-stylesheet" rel="internal" href="https://developer.mozilla.org/en/XML/xml-stylesheet" class="new ">xml-stylesheet</a>&nbsp;?&gt;).</p>
<p>Also inherits methods and properties from <a class="internal" title="En/DOM/Node" rel="internal" href="https://developer.mozilla.org/en/DOM/Node"><code>Node</code></a>.</p><br><br>
Documentation for this class was provided by <a href="https://developer.mozilla.org/en/DOM/ProcessingInstruction">MDN</a>. */
@:native("ProcessingInstruction")
extern class ProcessingInstruction extends Node
{
	/** Setter throws DOMException. */
	var data : String;

	var sheet(default,null) : StyleSheet;

	var target(default,null) : String;

}
