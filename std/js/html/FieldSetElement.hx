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

// This file is generated from mozilla\HTMLFieldSetElement.webidl. Do not edit!

package js.html;

/**
	The `HTMLFieldSetElement` interface has special properties and methods (beyond the regular `HTMLElement` interface it also has available to it by inheritance) for manipulating the layout and presentation of field-set elements.

	Documentation [HTMLFieldSetElement](https://developer.mozilla.org/en-US/docs/Web/API/HTMLFieldSetElement) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/HTMLFieldSetElement$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/HTMLFieldSetElement>
**/
@:native("HTMLFieldSetElement")
extern class FieldSetElement extends Element {
	
	/**
		A `Boolean` reflecting the `disabled` HTML attribute, indicating whether the user can interact with the control.
	**/
	var disabled : Bool;
	
	/**
		An `HTMLFormControlsCollection` or `HTMLCollection` referencing the containing form element, if this element is in a form.
		
		 If the field set is not a descendant of a form element, then the attribute can be the ID of any form element in the same document it is related to, or the `null` value if none matches.
	**/
	var form(default,null) : FormElement;
	
	/**
		A `DOMString` reflecting the `name` HTML attribute, containing the name of the field set, used for submitting the form.
	**/
	var name : String;
	
	/**
		The `DOMString` `"fieldset"`.
	**/
	var type(default,null) : String;
	
	/**
		The elements belonging to this field set. The type of this property depends on the version of the spec that is implemented by the browser.
	**/
	var elements(default,null) : HTMLCollection;
	
	/**
		A `Boolean` `false`, because `fieldset` objects are never candidates for constraint validation.
	**/
	var willValidate(default,null) : Bool;
	
	/**
		A `ValidityState` representing the validity states that this element is in.
	**/
	var validity(default,null) : ValidityState;
	
	/**
		A `DOMString` representing a localized message that describes the validation constraints that the element does not satisfy (if any). This is the empty string if the element is not a candidate for constraint validation (`willValidate` is `false`), or it satisfies its constraints.
	**/
	var validationMessage(default,null) : String;
	
	
	/**
		Always returns `true` because `fieldset` objects are never candidates for constraint validation.
	**/
	function checkValidity() : Bool;
	function reportValidity() : Bool;
	
	/**
		Sets a custom validity message for the field set. If this message is not the empty string, then the field set is suffering from a custom validity error, and does not validate.
	**/
	function setCustomValidity( error : String ) : Void;
}