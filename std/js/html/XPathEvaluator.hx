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

/** <div><div>

<a rel="custom" href="http://mxr.mozilla.org/mozilla-central/source/dom/interfaces/xpath/nsIDOMXPathEvaluator.idl"><code>dom/interfaces/xpath/nsIDOMXPathEvaluator.idl</code></a><span><a rel="internal" href="https://developer.mozilla.org/en/Interfaces/About_Scriptable_Interfaces" title="en/Interfaces/About_Scriptable_Interfaces">Scriptable</a></span></div><span>This interface is used to evaluate XPath expressions against a DOM node.</span><div>Inherits from: <code><a rel="custom" href="https://developer.mozilla.org/en/XPCOM_Interface_Reference/nsISupports">nsISupports</a></code>
<span>Last changed in Gecko 1.7 
</span></div></div>
<p></p>
<p>Implemented by: <code>@mozilla.org/dom/xpath-evaluator;1</code>. To create an instance, use:</p>
<pre class="eval">var domXPathEvaluator = Components.classes["@mozilla.org/dom/xpath-evaluator;1"]
                        .createInstance(Components.interfaces.nsIDOMXPathEvaluator);
</pre><br><br>
Documentation for this class was provided by <a href="https://developer.mozilla.org/en/XPCOM_Interface_Reference/nsIDOMXPathEvaluator">MDN</a>. */
@:native("XPathEvaluator")
extern class XPathEvaluator
{
	function new() : Void;

	function createExpression( expression : String, resolver : XPathNSResolver ) : XPathExpression;

	function createNSResolver( nodeResolver : Node ) : XPathNSResolver;

	function evaluate( expression : String, contextNode : Node, resolver : XPathNSResolver, type : Int, inResult : XPathResult ) : XPathResult;

}
