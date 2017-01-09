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

// This file is generated from mozilla\HTMLSelectElement.webidl. Do not edit!

package js.html;

/**
	The `HTMLSelectElement` interface represents a `select` HTML Element. These elements also share all of the properties and methods of other HTML elements via the `HTMLElement` interface.

	Documentation [HTMLSelectElement](https://developer.mozilla.org/en-US/docs/Web/API/HTMLSelectElement) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/HTMLSelectElement$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/HTMLSelectElement>
**/
@:native("HTMLSelectElement")
extern class SelectElement extends Element implements ArrayAccess<Element>
{
	
	/**
		Is a `Boolean` that reflects the `autofocus` HTML attribute, which indicates whether the control should have input focus when the page loads, unless the user overrides it, for example by typing in a different control. Only one form-associated element in a document can have this attribute specified. `2.0`
	**/
	var autofocus : Bool;
	
	/**
		Is a `Boolean` that reflects the `disabled` HTML attribute, which indicates whether the control is disabled. If it is disabled, it does not accept clicks.
	**/
	var disabled : Bool;
	
	/**
		Returns a `HTMLFormElement` representing the form that this element is associated with. If the element is not associated with of a `form` element, then it returns `null`.
	**/
	var form(default,null) : FormElement;
	
	/**
		Is a `Boolean` that reflects the `multiple` HTML attribute, which indicates whether multiple items can be selected.
	**/
	var multiple : Bool;
	
	/**
		Is a `DOMString` that reflects the `name` HTML attribute, containing the name of this control used by servers and DOM search functions.
	**/
	var name : String;
	
	/**
		Is a `Boolean` that reflects the `required` HTML attribute, which indicates whether the user is required to select a value before submitting the form. `2.0`
	**/
	var required : Bool;
	
	/**
		Is a `long` that reflects the `size` HTML attribute, which contains the number of visible items in the control. The default is 1, unless `multiple` is true, in which case it is 4.
	**/
	var size : Int;
	
	/**
		Returns a `DOMString` the form control's type. When `multiple` is `true`, it returns `"select-multiple"`; otherwise, it returns `"select-one"`.
	**/
	var type(default,null) : String;
	
	/**
		Returns a `HTMLOptionsCollection` containing the set of `option` elements contained by this element.
	**/
	var options(default,null) : HTMLOptionsCollection;
	
	/**
		Is a `unsigned long` representing the number of `option` elements in this `select` element.
	**/
	var length : Int;
	
	/**
		Returns a live `HTMLCollection` containing the set of options that are selected.
	**/
	var selectedOptions(default,null) : HTMLCollection;
	
	/**
		Is a `long` that reflects the index of the first selected `option` element. The value `-1` indicates no element is selected.
	**/
	var selectedIndex : Int;
	
	/**
		Is a `DOMString` representing the value of the form control (the first selected option).
	**/
	var value : String;
	
	/**
		Is a `Boolean` that indicates whether the button is a candidate for constraint validation. It is false if any conditions bar it from constraint validation.
	**/
	var willValidate(default,null) : Bool;
	
	/**
		Returns a `ValidityState` representing the validity state that this control is in.
	**/
	var validity(default,null) : ValidityState;
	
	/**
		Returns a `DOMString` containing a localized message that describes the validation constraints that the control does not satisfy (if any). This attribute is the empty string if the control is not a candidate for constraint validation (`willValidate` is false), or it satisfies its constraints.
	**/
	var validationMessage(default,null) : String;
	
	
	/**
		Gets an item from the options collection for this `select` element. You can also access an item by specifying the index in array-style brackets or parentheses, without calling this method explicitly.
	**/
	function item( index : Int ) : Element;
	
	/**
		Gets the item in the options collection with the specified name. The name string can match either the `id` or the `name` attribute of an option node. You can also access an item by specifying the name in array-style brackets or parentheses, without calling this method explicitly.
	**/
	function namedItem( name : String ) : OptionElement;
	/** @throws DOMError */
	
	/**
		Adds an element to the collection of `option` elements for this `select` element.
	**/
	function add( element : haxe.extern.EitherType<OptionElement,OptGroupElement>, ?before : haxe.extern.EitherType<Element,Int> ) : Void;
	
	/**
		Checks whether the element has any constraints and whether it satisfies them. If the element fails its constraints, the browser fires a cancelable `invalid` event at the element (and returns `false`).
	**/
	function checkValidity() : Bool;
	
	/**
		Sets the custom validity message for the selection element to the specified message. Use the empty string to indicate that the element does not have a custom validity error.
	**/
	function setCustomValidity( error : String ) : Void;
}