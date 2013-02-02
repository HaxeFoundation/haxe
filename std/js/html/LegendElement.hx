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

/** DOM&nbsp;Legend objects inherit all of the properties and methods of DOM <a href="https://developer.mozilla.org/en/DOM/HTMLElement" title="en/DOM/HTMLElement" rel="internal">HTMLElement</a>, and also expose the <a title="http://www.w3.org/TR/html5/forms.html#htmllegendelement" class=" external" rel="external nofollow" href="http://www.w3.org/TR/html5/forms.html#htmllegendelement" target="_blank">HTMLLegendElement</a> 
<span><a rel="custom" href="https://developer.mozilla.org/en/HTML/HTML5">HTML5</a></span> (or <a class=" external" title="http://www.w3.org/TR/DOM-Level-2-HTML/html.html#ID-21482039" rel="external" href="http://www.w3.org/TR/DOM-Level-2-HTML/html.html#ID-21482039" target="_blank">HTMLLegendElement</a> 
<span><a rel="custom" href="https://developer.mozilla.org/en/HTML">HTML 4</a></span>) interface.<br><br>
Documentation for this class was provided by <a href="https://developer.mozilla.org/en/DOM/HTMLLegendElement">MDN</a>. */
@:native("HTMLLegendElement")
extern class LegendElement extends Element
{
	/** Alignment relative to the form set. 

<span title="">Obsolete</span> in 
<span><a rel="custom" href="https://developer.mozilla.org/en/HTML/HTML5">HTML5</a></span>, 

<span class="deprecatedInlineTemplate" title="">Deprecated</span>

 in 
<span>HTML 4.01</span> */
	var align : String;

	/** The form that this legend belongs to. If the legend has a fieldset element as its parent, then this attribute returns the same value as the <strong>form</strong> attribute on the parent fieldset element. Otherwise, it returns null. */
	var form (default,null) : FormElement;

}
