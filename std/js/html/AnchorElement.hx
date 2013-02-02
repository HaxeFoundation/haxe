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

/** DOM anchor elements expose the <a target="_blank" href="http://www.w3.org/TR/html5/text-level-semantics.html#htmlanchorelement" rel="external nofollow" class=" external" title="http://www.w3.org/TR/html5/text-level-semantics.html#htmlanchorelement">HTMLAnchorElement</a> (or <span><a href="https://developer.mozilla.org/en/HTML" rel="custom nofollow">HTML 4</a></span> <a target="_blank" title="http://www.w3.org/TR/DOM-Level-2-HTML/html.html#ID-48250443" href="http://www.w3.org/TR/DOM-Level-2-HTML/html.html#ID-48250443" rel="external nofollow" class=" external"><code>HTMLAnchorElement</code></a>) interface, which provides special properties and methods (beyond the regular <a href="https://developer.mozilla.org/en/DOM/element" rel="internal">element</a> object interface they also have available to them by inheritance) for manipulating the layout and presentation of hyperlink elements.<br><br>
Documentation for this class was provided by <a href="https://developer.mozilla.org/en/DOM/HTMLAnchorElement">MDN</a>. */
@:native("HTMLAnchorElement")
extern class AnchorElement extends Element
{
	/** The character encoding of the linked resource. 

<span title="">Obsolete</span> in 
<span><a rel="custom" href="https://developer.mozilla.org/en/HTML/HTML5">HTML5</a></span> */
	var charset : String;

	/** Comma-separated list of coordinates. 

<span title="">Obsolete</span> in 
<span><a rel="custom" href="https://developer.mozilla.org/en/HTML/HTML5">HTML5</a></span> */
	var coords : String;

	var download : String;

	/** The fragment identifier (including the leading hash mark (#)), if any, in the referenced URL. */
	var hash : String;

	/** The hostname and port (if it's not the default port) in the referenced URL. */
	var host : String;

	/** The hostname in the referenced URL. */
	var hostname : String;

	/** Reflects the 

<code><a rel="custom" href="https://developer.mozilla.org/en/HTML/Element/a#attr-href">href</a></code>
 HTML attribute, containing a valid URL of a linked resource. */
	var href : String;

	/** Reflects the 

<code><a rel="custom" href="https://developer.mozilla.org/en/HTML/Element/a#attr-hreflang">hreflang</a></code>
 HTML&nbsp;attribute, indicating the language of the linked resource. */
	var hreflang : String;

	/** Anchor name. 

<span title="">Obsolete</span> in 
<span><a rel="custom" href="https://developer.mozilla.org/en/HTML/HTML5">HTML5</a></span> */
	var name : String;

	var origin (default,null) : String;

	/** The path name component, if any, of the referenced URL. */
	var pathname : String;

	var ping : String;

	/** The port component, if any, of the referenced URL. */
	var port : String;

	/** The protocol component (including trailing colon (:)), of the referenced URL. */
	var protocol : String;

	/** Reflects the 

<code><a rel="custom" href="https://developer.mozilla.org/en/HTML/Element/a#attr-rel">rel</a></code>
 HTML attribute, specifying the relationship of the target object to the link object. */
	var rel : String;

	/** Reverse link type. 

<span title="">Obsolete</span> in 
<span><a rel="custom" href="https://developer.mozilla.org/en/HTML/HTML5">HTML5</a></span> */
	var rev : String;

	/** The search element (including leading question mark (?)), if any, of the referenced URL */
	var search : String;

	/** The shape of the active area. 

<span title="">Obsolete</span> in 
<span><a rel="custom" href="https://developer.mozilla.org/en/HTML/HTML5">HTML5</a></span> */
	var shape : String;

	/** Reflectst the 

<code><a rel="custom" href="https://developer.mozilla.org/en/HTML/Element/a#attr-target">target</a></code>
 HTML attribute, indicating where to display the linked resource. */
	var target : String;

	/** Same as the <strong><a title="https://developer.mozilla.org/En/DOM/Node.textContent" rel="internal" href="https://developer.mozilla.org/En/DOM/Node.textContent">textContent</a></strong> property. */
	var text (default,null) : String;

	/** Reflects the 

<code><a rel="custom" href="https://developer.mozilla.org/en/HTML/Element/a#attr-type">type</a></code>
 HTML attribute, indicating the MIME type of the linked resource. */
	var type : String;

	function toString() : String;

}
