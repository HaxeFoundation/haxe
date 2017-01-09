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

// This file is generated from mozilla\HTMLLabelElement.webidl. Do not edit!

package js.html;

/**
	The `HTMLLabelElement` interface gives access to properties specific to `label` elements. It inherits methods and properties from the base `HTMLElement` interface.

	Documentation [HTMLLabelElement](https://developer.mozilla.org/en-US/docs/Web/API/HTMLLabelElement) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/HTMLLabelElement$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/HTMLLabelElement>
**/
@:native("HTMLLabelElement")
extern class LabelElement extends Element
{
	
	/**
		Is a `HTMLFormElement` object representing the form with which the labeled control is associated, or `null` if there is no associated control, or if that control isn't associated with a form. In other words, this is just a shortcut for `HTMLLabelElement.control.form`.
	**/
	var form(default,null) : FormElement;
	
	/**
		Is a string containing the ID of the labeled control. This reflects the `for` attribute.
	**/
	var htmlFor : String;
	
	/**
		Is a `HTMLElement` representing the control with which the label is associated.
	**/
	var control(default,null) : Element;
	
}