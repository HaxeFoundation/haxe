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

@:native("HTMLOutputElement")
extern class OutputElement extends Element
{
	/** The default value of the element, initially the empty string. */
	var defaultValue : String;

	/** Indicates the control's form owner, reflecting the 

<code><a rel="custom" href="https://developer.mozilla.org/en/HTML/Element/output#attr-form">form</a></code>
&nbsp;HTML&nbsp;attribute if it is defined. */
	var form(default,null) : FormElement;

	/** Reflects the 

<code><a rel="custom" href="https://developer.mozilla.org/en/HTML/Element/output#attr-for">for</a></code>
 HTML attribute, containing a list of IDs of other elements in the same document that contribute to (or otherwise affect) the calculated <strong>value</strong>. */
	var htmlFor : DOMSettableTokenList;

	/** A list of label elements associated with this output element. */
	var labels(default,null) : NodeList;

	/** Reflects the 

<code><a rel="custom" href="https://developer.mozilla.org/en/HTML/Element/output#attr-name">name</a></code>
 HTML attribute, containing the name for the control that is submitted with form data. */
	var name : String;

	/** Must be the string <code>output</code>. */
	var type(default,null) : String;

	/** A localized message that describes the validation constraints that the control does not satisfy (if any). This is the empty string if the control is not a candidate for constraint validation (<strong>willValidate</strong> is false), or it satisfies its constraints. */
	var validationMessage(default,null) : String;

	/** The validity states that this element is in. */
	var validity(default,null) : ValidityState;

	/** The value of the contents of the elements. Behaves like the <strong><a title="En/DOM/Node.textContent" rel="internal" href="https://developer.mozilla.org/En/DOM/Node.textContent">textContent</a></strong> property. */
	var value : String;

	/** <p>      in Gecko 2.0. Indicates whether the element is a candidate for constraint validation. It is false if any conditions bar it from constraint validation. (See <a rel="external" href="https://bugzilla.mozilla.org/show_bug.cgi?id=604673" class="external" title="">
bug 604673</a>
.)</p> <p>The standard behavior is to always return false because <code>output</code> objects are never candidates for constraint validation.</p> */
	var willValidate(default,null) : Bool;

	function checkValidity() : Bool;

	function setCustomValidity( error : String ) : Void;

}
