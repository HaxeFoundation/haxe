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

// This file is generated from mozilla\HTMLObjectElement.webidl. Do not edit!

package js.html;

/**
	The `HTMLObjectElement` interface provides special properties and methods (beyond those on the `HTMLElement` interface it also has available to it by inheritance) for manipulating the layout and presentation of `object` element, representing external resources.

	Documentation [HTMLObjectElement](https://developer.mozilla.org/en-US/docs/Web/API/HTMLObjectElement) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/HTMLObjectElement$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/HTMLObjectElement>
**/
@:native("HTMLObjectElement")
extern class ObjectElement extends Element
{
	
	/**
		Is a `DOMString` that reflects the `data` HTML attribute, specifying the address of a resource's data.
	**/
	var data : String;
	
	/**
		Is a `DOMString` that reflects the `type` HTML attribute, specifying the MIME type of the resource.
	**/
	var type : String;
	
	/**
		Is a `Boolean` that reflects the `typeMustMatch` HTML attribute, indicating if the resource specified by `data` must only be played if it matches the `type` attribute.
	**/
	var typeMustMatch : Bool;
	
	/**
		Is a `DOMString` that reflects the `name` HTML attribute, specifying the name of the browsing context.
	**/
	var name : String;
	
	/**
		Is a `DOMString` that reflects the `usemap` HTML attribute, specifying a `map` element to use.
	**/
	var useMap : String;
	
	/**
		Retuns a `HTMLFormElement` representing the object element's form owner, or null if there isn't one.
	**/
	var form(default,null) : FormElement;
	
	/**
		Is a `DOMString` that reflects the `width` HTML attribute, specifying the displayed width of the resource in CSS pixels.
	**/
	var width : String;
	
	/**
		Is a `DOMString` that reflects the `height` HTML attribute, specifying the displayed height of the resource in CSS pixels.
	**/
	var height : String;
	
	/**
		Returns a `Document` representing the active document of the object element's nested browsing context, if any; otherwise `null`.
	**/
	var contentDocument(default,null) : HTMLDocument;
	
	/**
		Returns a `WindowProxy` representing the window proxy of the object element's nested browsing context, if any; otherwise `null`.
	**/
	var contentWindow(default,null) : Window;
	
	/**
		Returns a `Boolean` that indicates whether the element is a candidate for constraint validation. Always `false` for `HTMLObjectElement` objects.
	**/
	var willValidate(default,null) : Bool;
	
	/**
		Returns a `ValidityState` with the validity states that this element is in.
	**/
	var validity(default,null) : ValidityState;
	
	/**
		Returns a `DOMString` representing a localized message that describes the validation constraints that the control does not satisfy (if any). This is the empty string if the control is not a candidate for constraint validation (`willValidate` is `false`), or it satisfies its constraints.
	**/
	var validationMessage(default,null) : String;
	
	/**
		Is a `DOMString` representing an enumerated property indicating alignment of the element's contents with respect to the surrounding context. The possible values are `"left"`, `"right"`, `"justify"`, and `"center"`.
	**/
	var align : String;
	
	/**
		Is a `DOMString` that reflects the `archive` HTML attribute, containing a list of archives for resources for this object.
	**/
	var archive : String;
	
	/**
		Is a `DOMString` representing the name of an applet class file, containing either the applet's subclass, or the path to get to the class, including the class file itself.
	**/
	var code : String;
	
	/**
		Is a `Boolean` that reflects the `declare` HTML attribute, indicating that this is a declaration, not an instantiation, of the object.
	**/
	var declare : Bool;
	
	/**
		Is a `long` representing the horizontal space in pixels around the control.
	**/
	var hspace : Int;
	
	/**
		Is a `DOMString` that reflects the `standby` HTML attribute, specifying a message to display while the object loads.
	**/
	var standby : String;
	
	/**
		Is a `long` representing the horizontal space in pixels around the control.
	**/
	var vspace : Int;
	
	/**
		Is a `DOMString` that reflects the `codebase` HTML attribute, specifying the base path to use to resolve relative URIs.
	**/
	var codeBase : String;
	
	/**
		Is a `DOMString` that reflects the `codetype` HTML attribute, specifying the content type of the data.
	**/
	var codeType : String;
	
	/**
		Is a `DOMString` that reflects the `border` HTML attribute, specifying the width of a border around the object.
	**/
	var border : String;
	
	
	/**
		Retuns a `Boolean` that always is `true`, because `object` objects are never candidates for constraint validation.
	**/
	function checkValidity() : Bool;
	
	/**
		Sets a custom validity message for the element. If this message is not the empty string, then the element is suffering from a custom validity error, and does not validate.
	**/
	function setCustomValidity( error : String ) : Void;
	function getSVGDocument() : HTMLDocument;
}