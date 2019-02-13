/*
 * Copyright (C)2005-2019 Haxe Foundation
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

// This file is generated from mozilla\HTMLFormElement.webidl. Do not edit!

package js.html;

/**
	The `HTMLFormElement` interface represents a `form` element in the DOM; it allows access to and in some cases modification of aspects of the form, as well as access to its component elements.

	Documentation [HTMLFormElement](https://developer.mozilla.org/en-US/docs/Web/API/HTMLFormElement) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/HTMLFormElement$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/HTMLFormElement>
**/
@:native("HTMLFormElement")
extern class FormElement extends Element implements ArrayAccess<Element> {
	
	/**
		A `DOMString` reflecting the value of the form's `accept-charset` HTML attribute, representing the character encoding that the server accepts.
	**/
	var acceptCharset : String;
	
	/**
		A `DOMString` reflecting the value of the form's `action` HTML attribute, containing the URI of a program that processes the information submitted by the form.
	**/
	var action : String;
	
	/**
		A `DOMString` reflecting the value of the form's `autocomplete` HTML attribute, indicating whether the controls in this form can have their values automatically populated by the browser.
	**/
	var autocomplete : String;
	
	/**
		A `DOMString` reflecting the value of the form's `enctype` HTML attribute, indicating the type of content that is used to transmit the form to the server. Only specified values can be set. The two properties are synonyms.
	**/
	var enctype : String;
	
	/**
		A `DOMString` reflecting the value of the form's `enctype` HTML attribute, indicating the type of content that is used to transmit the form to the server. Only specified values can be set. The two properties are synonyms.
	**/
	var encoding : String;
	
	/**
		A `DOMString` reflecting the value of the form's `method` HTML attribute, indicating the HTTP method used to submit the form. Only specified values can be set.
	**/
	var method : String;
	
	/**
		A `DOMString` reflecting the value of the form's `name` HTML attribute, containing the name of the form.
	**/
	var name : String;
	
	/**
		A `Boolean` reflecting the value of the form's  `novalidate` HTML attribute, indicating whether the form should not be validated.
	**/
	var noValidate : Bool;
	
	/**
		A `DOMString` reflecting the value of the form's `target` HTML attribute, indicating where to display the results received from submitting the form.
	**/
	var target : String;
	
	/**
		A `HTMLFormControlsCollection` holding all form controls belonging to this form element.
	**/
	var elements(default,null) : HTMLCollection;
	
	/**
		A `long` reflecting  the number of controls in the form.
	**/
	var length(default,null) : Int;
	
	
	/**
		Submits the form to the server.
		@throws DOMError
	**/
	function submit() : Void;
	
	/**
		Resets the form to its initial state.
	**/
	function reset() : Void;
	
	/**
		Returns `true` if the element's child controls are subject to constraint validation and satisfy those contraints; returns `false` if some controls do not satisfy their constraints. Fires an event named `invalid` at any control that does not satisfy its constraints; such controls are considered invalid if the event is not canceled. It is up to the programmer to decide how to respond to `false`.
	**/
	function checkValidity() : Bool;
	
	/**
		Returns `true` if the element's child controls satisfy their validation constraints. When `false` is returned, cancelable `invalid` events are fired for each invalid child and validation problems are reported to the user.
	**/
	function reportValidity() : Bool;
}