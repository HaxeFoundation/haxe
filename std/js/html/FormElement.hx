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

/** <p><code>FORM</code> elements share all of the properties and methods of other HTML elements described in the <a title="en/DOM/element" rel="internal" href="https://developer.mozilla.org/en/DOM/element">element</a> section.</p>
<p>This interface provides methods to create and modify <code>FORM</code> elements using the DOM.</p><br><br>
Documentation for this class was provided by <a href="https://developer.mozilla.org/en/DOM/HTMLFormElement">MDN</a>. */
@:native("HTMLFormElement")
extern class FormElement extends Element
{
	/** Reflects the 

<code><a rel="custom" href="https://developer.mozilla.org/en/HTML/Element/form#attr-accept-charset">accept-charset</a></code>
&nbsp;HTML&nbsp;attribute, containing a list of character encodings that the server accepts. */
	var acceptCharset : String;

	/** Reflects the 

<code><a rel="custom" href="https://developer.mozilla.org/en/HTML/Element/form#attr-action">action</a></code>
&nbsp;HTML&nbsp;attribute, containing the URI&nbsp;of a program that processes the information submitted by the form. */
	var action : String;

	/** Reflects the 

<code><a rel="custom" href="https://developer.mozilla.org/en/HTML/Element/form#attr-autocomplete">autocomplete</a></code>
 HTML&nbsp;attribute, containing a string that indicates whether the controls in this form can have their values automatically populated by the browser. */
	var autocomplete : String;

	/** All the form controls belonging to this form element. */
	var elements (default,null) : HTMLCollection;

	/** Synonym for <strong>enctype</strong>. */
	var encoding : String;

	/** Reflects the 

<code><a rel="custom" href="https://developer.mozilla.org/en/HTML/Element/form#attr-enctype">enctype</a></code>
&nbsp;HTML&nbsp;attribute, indicating the type of content that is used to transmit the form to the server. Only specified values can be set. */
	var enctype : String;

	/** The number of controls in the form. */
	var length (default,null) : Int;

	/** Reflects the 

<code><a rel="custom" href="https://developer.mozilla.org/en/HTML/Element/form#attr-method">method</a></code>
&nbsp;HTML&nbsp;attribute, indicating the HTTP&nbsp;method used to submit the form. Only specified values can be set. */
	var method : String;

	/** Reflects the 

<code><a rel="custom" href="https://developer.mozilla.org/en/HTML/Element/form#attr-name">name</a></code>
&nbsp;HTML&nbsp;attribute, containing the name of the form. */
	var name : String;

	/** Reflects the 

<code><a rel="custom" href="https://developer.mozilla.org/en/HTML/Element/form#attr-novalidate">novalidate</a></code>
 HTML attribute, indicating that the form should not be validated. */
	var noValidate : Bool;

	/** Reflects the 

<code><a rel="custom" href="https://developer.mozilla.org/en/HTML/Element/form#attr-target">target</a></code>
 HTML attribute, indicating where to display the results received from submitting the form. */
	var target : String;

	function checkValidity() : Bool;

	function reset() : Void;

	function submit() : Void;

}
