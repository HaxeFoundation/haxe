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

/** <strong>Note:</strong>&nbsp;This page describes the Keygen Element interface as specified, not as currently implemented by Gecko. See <a rel="external" href="https://bugzilla.mozilla.org/show_bug.cgi?id=101019" class="external" title="">
bug 101019</a>
 for details and status.<br><br>
Documentation for this class was provided by <a href="https://developer.mozilla.org/en/DOM/HTMLKeygenElement">MDN</a>. */
@:native("HTMLKeygenElement")
extern class KeygenElement extends Element
{
	/** Reflects the 

<code><a rel="custom" href="https://developer.mozilla.org/en/HTML/Element/keygen#attr-autofocus">autofocus</a></code>
&nbsp;HTML attribute, indicating that the form control should have input focus when the page loads. */
	var autofocus : Bool;

	/** Reflects the 

<code><a rel="custom" href="https://developer.mozilla.org/en/HTML/Element/keygen#attr-challenge">challenge</a></code>
 HTML&nbsp;attribute, containing a challenge string that is packaged with the submitted key. */
	var challenge : String;

	/** Reflects the 

<code><a rel="custom" href="https://developer.mozilla.org/en/HTML/Element/keygen#attr-disabled">disabled</a></code>
&nbsp;HTML attribute, indicating that the control is not available for interaction. */
	var disabled : Bool;

	/** Indicates the control's form owner, reflecting the 

<code><a rel="custom" href="https://developer.mozilla.org/en/HTML/Element/keygen#attr-form">form</a></code>
&nbsp;HTML&nbsp;attribute if it is defined. */
	var form (default,null) : FormElement;

	/** Reflects the 

<code><a rel="custom" href="https://developer.mozilla.org/en/HTML/Element/keygen#attr-keytype">keytype</a></code>
 HTML&nbsp;attribute, containing the type of key used. */
	var keytype : String;

	/** A list of label elements associated with this keygen element. */
	var labels (default,null) : NodeList;

	/** Reflects the 

<code><a rel="custom" href="https://developer.mozilla.org/en/HTML/Element/keygen#attr-name">name</a></code>
 HTML attribute, containing the name for the control that is submitted with form data. */
	var name : String;

	/** Must be the value <code>keygen</code>. */
	var type (default,null) : String;

	/** A localized message that describes the validation constraints that the control does not satisfy (if any). This is the empty string if the control is not a candidate for constraint validation (<strong>willValidate</strong> is false), or it satisfies its constraints. */
	var validationMessage (default,null) : String;

	/** The validity states that this element is in. */
	var validity (default,null) : ValidityState;

	/** Always false because <code>keygen</code> objects are never candidates for constraint validation. */
	var willValidate (default,null) : Bool;

	function checkValidity() : Bool;

	function setCustomValidity( error : String ) : Void;

}
