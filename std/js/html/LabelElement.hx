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

/** DOM Label objects inherit all of the properties and methods of DOM <a title="en/DOM/element" rel="internal" href="https://developer.mozilla.org/en/DOM/element">element</a>, and also expose the <a title="http://dev.w3.org/html5/spec/forms.html#htmllabelelement" class=" external" rel="external" href="http://dev.w3.org/html5/spec/forms.html#htmllabelelement" target="_blank">HTMLLabelElement</a>(or 
<span><a rel="custom" href="https://developer.mozilla.org/en/HTML">HTML 4</a></span> <a class=" external" title="http://www.w3.org/TR/DOM-Level-2-HTML/html.html#ID-13691394" rel="external" href="http://www.w3.org/TR/DOM-Level-2-HTML/html.html#ID-13691394" target="_blank">HTMLLabelElement</a>) interface.<br><br>
Documentation for this class was provided by <a href="https://developer.mozilla.org/en/DOM/HTMLLabelElement">MDN</a>. */
@:native("HTMLLabelElement")
extern class LabelElement extends Element
{
	/** The labeled control. */
	var control (default,null) : Element;

	/** The form owner of this label. */
	var form (default,null) : FormElement;

	/** The ID of the labeled control. Reflects the 

<code><a rel="custom" href="https://developer.mozilla.org/en/HTML/Element/label#attr-for">for</a></code>
 attribute. */
	var htmlFor : String;

}
