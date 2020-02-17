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

// This file is generated from mozilla\HTMLOutputElement.webidl. Do not edit!

package js.html;

/**
	The `HTMLOutputElement` interface provides properties and methods (beyond those inherited from `HTMLElement`) for manipulating the layout and presentation of `output` elements.

	Documentation [HTMLOutputElement](https://developer.mozilla.org/en-US/docs/Web/API/HTMLOutputElement) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/HTMLOutputElement$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/HTMLOutputElement>
**/
@:native("HTMLOutputElement")
extern class OutputElement extends Element {
	
	/**
		A `DOMTokenList` reflecting the `for` HTML attribute, containing a list of IDs of other elements in the same document that contribute to (or otherwise affect) the calculated `value`.
	**/
	var htmlFor(default,null) : DOMTokenList;
	
	/**
		An `HTMLFormElement` indicating the form associated with the control, reflecting the `form` HTML attribute if it is defined.
	**/
	var form(default,null) : FormElement;
	
	/**
		A `DOMString` reflecting the `name` HTML attribute, containing the name for the control that is submitted with form data.
	**/
	var name : String;
	
	/**
		The `DOMString` `"output"`.
	**/
	var type(default,null) : String;
	
	/**
		A `DOMString` representing the default value of the element, initially the empty string.
	**/
	var defaultValue : String;
	
	/**
		A `DOMString` representing the value of the contents of the elements. Behaves like the `Node.textContent` property.
	**/
	var value : String;
	
	/**
		A `Boolean` indicating whether the element is a candidate for constraint validation.
	**/
	var willValidate(default,null) : Bool;
	
	/**
		A `ValidityState` representing the validity states that this element is in.
	**/
	var validity(default,null) : ValidityState;
	
	/**
		A `DOMString` representing a localized message that describes the validation constraints that the control does not satisfy (if any). This is the empty string if the control is not a candidate for constraint validation (`willValidate` is `false`), or it satisfies its constraints.
	**/
	var validationMessage(default,null) : String;
	
	/**
		A `NodeList` of `label` elements associated with the element.
	**/
	var labels(default,null) : NodeList;
	
	
	/**
		Checks the validity of the element and returns a `Boolean` holding the check result.
	**/
	function checkValidity() : Bool;
	
	/**
		Triggers an `invalid` event and evaluates its result. if the result is `true`, then the problems with the constraints of this element are reported to the user. When the problem is reported, the user agent may focus the element and change the scrolling position of the document or perform some other action that brings the element to the user's attention. User agents may report more than one constraint violation if this element suffers from multiple problems at once. If the element is not rendered, then the user agent may report the error for the running script instead of notifying the user.
	**/
	function reportValidity() : Bool;
	
	/**
		Sets a custom validity message for the element. If this message is not the empty string, then the element is suffering from a custom validity error, and does not validate.
	**/
	function setCustomValidity( error : String ) : Void;
}