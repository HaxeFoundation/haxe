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

/** <p>DOM&nbsp;<em>option</em> elements elements share all of the properties and methods of other HTML elements described in the <a title="en/DOM/element" rel="internal" href="/api/js/html/Element">element</a> section. They also have the specialized interface <a title="http://dev.w3.org/html5/spec/the-button-element.html#htmloptionelement" class=" external" rel="external" href="http://dev.w3.org/html5/spec/the-button-element.html#htmloptionelement" target="_blank">HTMLOptionElement</a> (or 
<span><a rel="custom" href="https://developer.mozilla.org/en/HTML">HTML 4</a></span> <a class="external" title="http://www.w3.org/TR/DOM-Level-2-HTML/html.html#ID-70901257" rel="external" href="http://www.w3.org/TR/DOM-Level-2-HTML/html.html#ID-70901257" target="_blank">HTMLOptionElement</a>).</p>
<p>No methods are defined on this interface.</p><br><br>
Documentation for this class was provided by <a href="https://developer.mozilla.org/en/DOM/HTMLOptionElement">MDN</a>. */
@:native("HTMLOptionElement")
extern class OptionElement extends Element
{
	/** Reflects the value of the 

<code><a rel="custom" href="https://developer.mozilla.org/en/HTML/Element/option#attr-selected">selected</a></code>
 HTML attribute. which indicates whether the option is selected by default. */
	var defaultSelected : Bool;

	/** Reflects the value of the 

<code><a rel="custom" href="https://developer.mozilla.org/en/HTML/Element/option#attr-disabled">disabled</a></code>
 HTML&nbsp;attribute, which indicates that the option is unavailable to be selected. An option can also be disabled if it is a child of an <code><a rel="custom" href="https://developer.mozilla.org/en/HTML/Element/optgroup">&lt;optgroup&gt;</a></code>
 element that is disabled. */
	var disabled : Bool;

	/** If the option is a descendent of a <code><a rel="custom" href="https://developer.mozilla.org/en/HTML/Element/select">&lt;select&gt;</a></code>
 element, then this property has the same value as the <code>form</code> property of the corresponding {{DomXref("HTMLSelectElement") object; otherwise, it is null. */
	var form(default,null) : FormElement;

	/** The position of the option within the list of options it belongs to, in tree-order. If the option is not part of a list of options, the value is 0. */
	var index(default,null) : Int;

	/** Reflects the value of the 

<code><a rel="custom" href="https://developer.mozilla.org/en/HTML/Element/option#attr-label">label</a></code>
 HTML attribute, which provides a label for the option. If this attribute isn't specifically set, reading it returns the element's text content. */
	var label : String;

	/** Indicates whether the option is selected. */
	var selected : Bool;

	/** Contains the text content of the element. Setter throws DOMException. */
	var text : String;

	/** Reflects the value of the 

<code><a rel="custom" href="https://developer.mozilla.org/en/HTML/Element/option#attr-value">value</a></code>
 HTML attribute, if it exists; otherwise reflects value of the <code><a rel="internal" href="https://developer.mozilla.org/Article_not_found?uri=en/DOM/textContent" class="new">textContent</a></code>
&nbsp;IDL&nbsp;attribute. */
	var value : String;

	function new() : Void;

}
