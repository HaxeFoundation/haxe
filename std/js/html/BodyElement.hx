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

/** DOM body elements expose the <a href="http://www.w3.org/TR/html5/sections.html#the-body-element" target="_blank" rel="external nofollow" class=" external" title="http://www.w3.org/TR/html5/sections.html#the-body-element">HTMLBodyElement</a> (or 
<span><a rel="custom" href="https://developer.mozilla.org/en/HTML">HTML 4</a></span> <a href="http://www.w3.org/TR/DOM-Level-2-HTML/html.html#ID-48250443" target="_blank" title="http://www.w3.org/TR/DOM-Level-2-HTML/html.html#ID-48250443" rel="external nofollow" class=" external"><code>HTMLBodyElement</code></a>) interface, which provides special properties (beyond the regular <code><a rel="custom" href="/api/js/html/Element">element</a></code>
 object interface they also have available to them by inheritance) for manipulating body elements.<br><br>
Documentation for this class was provided by <a href="https://developer.mozilla.org/en/DOM/HTMLBodyElement">MDN</a>. */
@:native("HTMLBodyElement")
extern class BodyElement extends Element
{
	/** Color of active hyperlinks. */
	var aLink : String;

	/** <p>URI for a background image resource.</p> <div class="note"><strong>Note:</strong> Starting in Gecko 7.0 (Firefox 7.0 / Thunderbird 7.0 / SeaMonkey 2.4)
, this value is no longer resolved as a URI; instead, it's treated as a simple string.</div> */
	var background : String;

	/** Background color for the document. */
	var bgColor : String;

	/** Color of unvisited links. */
	var link : String;

	/** Reflects the 

<code><a rel="custom" href="https://developer.mozilla.org/en/HTML/Element/body#attr-onbeforeunload">onbeforeunload</a></code>
 HTML&nbsp;attribute value for a function to call when the document is about to be unloaded. */
	var onbeforeunload : EventListener;

	/** Reflects the 

<code><a rel="custom" href="https://developer.mozilla.org/en/HTML/Element/body#attr-onhashchange">onhashchange</a></code>
 HTML&nbsp;attribute value for a function to call when the fragment identifier in the address of the document changes. */
	var onhashchange : EventListener;

	/** Reflects the 

<code><a rel="custom" href="https://developer.mozilla.org/en/HTML/Element/body#attr-onmessage">onmessage</a></code>
 HTML&nbsp;attribute value for a function to call when the document receives a message. */
	var onmessage : EventListener;

	/** Reflects the 

<code><a rel="custom" href="https://developer.mozilla.org/en/HTML/Element/body#attr-onoffline">onoffline</a></code>
 HTML&nbsp;attribute value for a function to call when network communication fails. */
	var onoffline : EventListener;

	/** Reflects the 

<code><a rel="custom" href="https://developer.mozilla.org/en/HTML/Element/body#attr-ononline">ononline</a></code>
 HTML&nbsp;attribute value for a function to call when network communication is restored. */
	var ononline : EventListener;

	/** Reflects the 

<code><a rel="custom" href="https://developer.mozilla.org/en/HTML/Element/body#attr-onpopstate">onpopstate</a></code>
 HTML&nbsp;attribute value for a function to call when the user has navigated session history. */
	var onpopstate : EventListener;

	/** Reflects the 

<code><a rel="custom" href="https://developer.mozilla.org/en/HTML/Element/body#attr-onresize">onresize</a></code>
 HTML&nbsp;attribute value for a function to call when the document has been resized. */
	var onresize : EventListener;

	/** Reflects the 

<code><a rel="custom" href="https://developer.mozilla.org/en/HTML/Element/body#attr-onpopstate">onpopstate</a></code>
 HTML&nbsp;attribute value for a function to call when the storage area has changed. */
	var onstorage : EventListener;

	/** Reflects the 

<code><a rel="custom" href="https://developer.mozilla.org/en/HTML/Element/body#attr-onunload">onunload</a></code>
 HTML&nbsp;attribute value for a function to call when when the document is going away. */
	var onunload : EventListener;

	/** Foreground color of text. */
	var text : String;

	/** Color of visited links. */
	var vLink : String;

}
