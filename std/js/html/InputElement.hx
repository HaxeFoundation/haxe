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

// This file is generated from mozilla/HTMLInputElement.webidl line 24:0. Do not edit!

package js.html;

@:native("HTMLInputElement")
extern class InputElement extends Element
{
	var accept : String;
	var alt : String;
	var autocomplete : String;
	var autofocus : Bool;
	var defaultChecked : Bool;
	var checked : Bool;
	var disabled : Bool;
	var form(default,null) : FormElement;
	var files(default,null) : FileList;
	var formAction : String;
	var formEnctype : String;
	var formMethod : String;
	var formNoValidate : Bool;
	var formTarget : String;
	var height : Int;
	var indeterminate : Bool;
	var list(default,null) : Element;
	var max : String;
	var maxLength : Int;
	var min : String;
	var multiple : Bool;
	var name : String;
	var pattern : String;
	var placeholder : String;
	var readOnly : Bool;
	var required : Bool;
	var size : Int;
	var src : String;
	var step : String;
	var type : String;
	var defaultValue : String;
	var value : String;
	var valueAsNumber : Float;
	var width : Int;
	var willValidate(default,null) : Bool;
	var validity(default,null) : ValidityState;
	var validationMessage(default,null) : String;
	var selectionStart : Int;
	var selectionEnd : Int;
	var selectionDirection : String;
	var align : String;
	var useMap : String;
	var textLength(default,null) : Int;
	
	/** @throws DOMError */
	function stepUp( ?n : Int = 1 ) : Void;
	/** @throws DOMError */
	function stepDown( ?n : Int = 1 ) : Void;
	function checkValidity() : Bool;
	function setCustomValidity( error : String ) : Void;
	function select() : Void;
	/** @throws DOMError */
	@:overload( function( replacement : String ) : Void {} )
	function setRangeText( replacement : String, start : Int, end : Int, ?selectionMode : SelectionMode = "preserve" ) : Void;
	/** @throws DOMError */
	function setSelectionRange( start : Int, end : Int, ?direction : String ) : Void;
}