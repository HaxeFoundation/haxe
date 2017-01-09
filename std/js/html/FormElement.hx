/*
 * Copyright (C)2005-2017 Haxe Foundation
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
	The `HTMLFormElement` interface provides methods to create and modify `form` elements.
		document.forms - returns an array of HTMLFormElement objects referencing all forms on the page.
		document.forms[index] - returns an HTMLFormElement object referencing the form at the specified index.
		document.forms['id'] - returns an HTMLFormElement object referencing the form with the specified id.
		document.forms['name'] - returns an HTMLFormElement object referencing the form with the specified name.

	Documentation [HTMLFormElement](https://developer.mozilla.org/en-US/docs/Web/API/HTMLFormElement) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/HTMLFormElement$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/HTMLFormElement>
**/
@:native("HTMLFormElement")
extern class FormElement extends Element implements ArrayAccess<Element>
{
	var acceptCharset : String;
	var action : String;
	var autocomplete : String;
	var enctype : String;
	var encoding : String;
	var method : String;
	var name : String;
	var noValidate : Bool;
	var target : String;
	var elements(default,null) : HTMLCollection;
	var length(default,null) : Int;
	
	/** @throws DOMError */
	function submit() : Void;
	function reset() : Void;
	function checkValidity() : Bool;
}