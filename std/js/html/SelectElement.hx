/*
 * Copyright (C)2005-2015 Haxe Foundation
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

// This file is generated from mozilla/HTMLSelectElement.webidl line 10:0. Do not edit!

package js.html;

@:native("HTMLSelectElement")
extern class SelectElement extends Element implements ArrayAccess<Element>
{
	var autofocus : Bool;
	var disabled : Bool;
	var form(default,null) : FormElement;
	var multiple : Bool;
	var name : String;
	var required : Bool;
	var size : Int;
	var type(default,null) : String;
	var options(default,null) : HTMLOptionsCollection;
	var length : Int;
	var selectedOptions(default,null) : HTMLCollection;
	var selectedIndex : Int;
	var value : String;
	var willValidate(default,null) : Bool;
	var validity(default,null) : ValidityState;
	var validationMessage(default,null) : String;
	
	function item( index : Int ) : Element;
	function namedItem( name : String ) : OptionElement;
	/** @throws DOMError */
	function add( element : haxe.extern.EitherType<OptionElement,OptGroupElement>, ?before : haxe.extern.EitherType<Element,Int> ) : Void;
	function checkValidity() : Bool;
	function setCustomValidity( error : String ) : Void;
}