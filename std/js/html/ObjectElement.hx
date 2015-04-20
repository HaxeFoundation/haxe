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

// This file is generated from mozilla/HTMLObjectElement.webidl line 17:0. Do not edit!

package js.html;

@:native("HTMLObjectElement")
extern class ObjectElement extends Element
{
	var data : String;
	var type : String;
	var typeMustMatch : Bool;
	var name : String;
	var useMap : String;
	var form(default,null) : FormElement;
	var width : String;
	var height : String;
	var contentDocument(default,null) : HTMLDocument;
	var contentWindow(default,null) : Window;
	var willValidate(default,null) : Bool;
	var validity(default,null) : ValidityState;
	var validationMessage(default,null) : String;
	var align : String;
	var archive : String;
	var code : String;
	var declare : Bool;
	var hspace : Int;
	var standby : String;
	var vspace : Int;
	var codeBase : String;
	var codeType : String;
	var border : String;
	
	function checkValidity() : Bool;
	function setCustomValidity( error : String ) : Void;
	function getSVGDocument() : HTMLDocument;
}