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

/** <p>A CDATA Section can be used within XML to include extended portions of unescaped text, such that the symbols &lt; and &amp; do not need escaping as they normally do within XML when used as text.</p>
<p>It takes the form:</p>
<pre class="eval">&lt;![CDATA[  ... ]]&gt;
</pre>
<p>For example:</p>
<pre class="eval">&lt;foo&gt;Here is a CDATA section: &lt;![CDATA[  &lt; &gt; &amp; ]]&gt; with all kinds of unescaped text. &lt;/foo&gt;
</pre>
<p>The only sequence which is not allowed within a CDATA section is the closing sequence of a CDATA section itself:</p>
<pre class="eval">&lt;![CDATA[  ]]&gt; will cause an error   ]]&gt;
</pre>
<p>Note that CDATA sections should not be used (without hiding) within HTML.</p>
<p>As a CDATASection has no properties or methods unique to itself and only directly implements the Text interface, one can refer to <a title="En/DOM/Text" rel="internal" href="https://developer.mozilla.org/En/DOM/Text">Text</a> to find its properties and methods.</p><br><br>
Documentation for this class was provided by <a href="https://developer.mozilla.org/en/DOM/CDATASection">MDN</a>. */
@:native("CDATASection")
extern class CDATASection extends Text
{
}
