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

/** DOM area objects expose the <a class=" external" title="http://www.w3.org/TR/html5/the-map-element.html#htmlareaelement" rel="external" href="http://www.w3.org/TR/html5/the-map-element.html#htmlareaelement" target="_blank">HTMLAreaElement</a> (or 
<span><a rel="custom" href="https://developer.mozilla.org/en/HTML">HTML 4</a></span> <a class=" external" title="http://www.w3.org/TR/DOM-Level-2-HTML/html.html#ID-26019118" rel="external" href="http://www.w3.org/TR/DOM-Level-2-HTML/html.html#ID-26019118" target="_blank"><code>HTMLAreaElement</code></a>) interface, which provides special properties and methods (beyond the regular <a href="/api/js/html/Element" rel="internal">element</a> object interface they also have available to them by inheritance) for manipulating the layout and presentation of area elements.<br><br>
Documentation for this class was provided by <a href="https://developer.mozilla.org/en/DOM/HTMLAreaElement">MDN</a>. */
@:native("HTMLAreaElement")
extern class AreaElement extends Element
{
	/** Reflects the 

<code><a rel="custom" href="https://developer.mozilla.org/en/HTML/Element/area#attr-alt">alt</a></code>
 HTML attribute, containing alternative text for the element. */
	var alt : String;

	/** Reflects the 

<code><a rel="custom" href="https://developer.mozilla.org/en/HTML/Element/area#attr-coords">coords</a></code>
 HTML attribute, containing coordinates to define the hot-spot region. */
	var coords : String;

	/** The fragment identifier (including the leading hash mark (#)), if any, in the referenced URL. */
	var hash(default,null) : String;

	/** The hostname and port (if it's not the default port) in the referenced URL. */
	var host(default,null) : String;

	/** The hostname in the referenced URL. */
	var hostname(default,null) : String;

	/** Reflects the 

<code><a rel="custom" href="https://developer.mozilla.org/en/HTML/Element/area#attr-href">href</a></code>
 HTML attribute, containing a valid URL&nbsp;of a linked resource. */
	var href : String;

	/** Indicates that this area is inactive. 

<span title="">Obsolete</span> in 
<span><a rel="custom" href="https://developer.mozilla.org/en/HTML/HTML5">HTML5</a></span> */
	var noHref : Bool;

	/** The path name component, if any, of the referenced URL. */
	var pathname(default,null) : String;

	var ping : String;

	/** The port component, if any, of the referenced URL. */
	var port(default,null) : String;

	/** The protocol component (including trailing colon (:)), of the referenced URL. */
	var protocol(default,null) : String;

	/** The search element (including leading question mark (?)), if any, of the referenced URL */
	var search(default,null) : String;

	/** Reflects the 

<code><a rel="custom" href="https://developer.mozilla.org/en/HTML/Element/area#attr-shape">shape</a></code>
 HTML&nbsp;attribute, indicating the shape of the hot-spot, limited to known values. */
	var shape : String;

	/** Reflects the 

<code><a rel="custom" href="https://developer.mozilla.org/en/HTML/Element/area#attr-target">target</a></code>
 HTML&nbsp;attribute, indicating the browsing context in which to open the linked resource. */
	var target : String;

}
