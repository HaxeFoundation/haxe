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

// This file is generated from mozilla\HTMLButtonElement.webidl. Do not edit!

package js.html;

/**
	The `HTMLButtonElement` interface provides properties and methods (beyond the `button` object interface it also has available to them by inheritance) for manipulating the layout and presentation of button elements.

	Documentation [HTMLButtonElement](https://developer.mozilla.org/en-US/docs/Web/API/HTMLButtonElement) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/HTMLButtonElement$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/HTMLButtonElement>
**/
@:native("HTMLButtonElement")
extern class ButtonElement extends Element
{
	
	/**
		Is a `Boolean` indicating whether or not the control should have input focus when the page loads, unless the user overrides it, for example by typing in a different control. Only one form-associated element in a document can have this attribute specified.
	**/
	var autofocus : Bool;
	
	/**
		Is a `Boolean` indicating whether or not the control is disabled, meaning that it does not accept any clicks.
	**/
	var disabled : Bool;
	
	/**
		Is a `HTMLFormElement` reflecting the form that this button is associated with. If the button is a descendant of a form element, then this attribute is the ID of that form element.
		
		 If the button is not a descendant of a form element, then the attribute can be the ID of any form element in the same document it is related to, or the `null` value if none matches.
	**/
	var form(default,null) : FormElement;
	
	/**
		Is a `DOMString` reflecting the URI of a resource that processes information submitted by the button. If specified, this attribute overrides the `action` attribute of the `form` element that owns this element.
	**/
	var formAction : String;
	var formEnctype : String;
	
	/**
		Is a `DOMString` reflecting the HTTP method that the browser uses to submit the form. If specified, this attribute overrides the `method` attribute of the `form` element that owns this element.
	**/
	var formMethod : String;
	
	/**
		Is a `Boolean` indicating that the form is not to be validated when it is submitted. If specified, this attribute overrides the `novalidate` attribute of the `form` element that owns this element.
	**/
	var formNoValidate : Bool;
	
	/**
		Is a `DOMString` reflecting a name or keyword indicating where to display the response that is received after submitting the form. If specified, this attribute overrides the `target` attribute of the `form` element that owns this element.
	**/
	var formTarget : String;
	
	/**
		Is a `DOMString` representing the name of the object when submitted with a form. {{HTMLVersionInline(5)}} If specified, it must not be the empty string.
	**/
	var name : String;
	
	/**
		Is a `DOMString` indicating the behavior of the button. This is an enumerated attribute with the following possible values:
		 
		  `"submit"`: The button submits the form. This is the default value if the attribute is not specified, {{HTMLVersionInline(5)}} or if it is dynamically changed to an empty or invalid value.
		  `"reset"`: The button resets the form.
		  `"button"`: The button does nothing.
		  `"menu"`: The button displays a menu. <em>(experimental)</em>
		 
		 
	**/
	var type : String;
	
	/**
		Is a `DOMString` representing the current form control value of the button.
	**/
	var value : String;
	
	/**
		Is a `Boolean` indicating whether the button is a candidate for constraint validation. It is `false` if any conditions bar it from constraint validation.
	**/
	var willValidate(default,null) : Bool;
	
	/**
		Is a `ValidityState` representing the validity states that this button is in.
	**/
	var validity(default,null) : ValidityState;
	
	/**
		Is a `DOMString` representing the localized message that describes the validation constraints that the control does not satisfy (if any). This attribute is the empty string if the control is not a candidate for constraint validation (`willValidate` is `false`), or it satisfies its constraints.
	**/
	var validationMessage(default,null) : String;
	
	function checkValidity() : Bool;
	function setCustomValidity( error : String ) : Void;
}