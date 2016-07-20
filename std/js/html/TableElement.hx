/*
 * Copyright (C)2005-2016 Haxe Foundation
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

// This file is generated from mozilla\HTMLTableElement.webidl line 16:0. Do not edit!

package js.html;

@:native("HTMLTableElement")
extern class TableElement extends Element
{
	var caption : TableCaptionElement;
	var tHead : TableSectionElement;
	var tFoot : TableSectionElement;
	var tBodies(default,null) : HTMLCollection;
	var rows(default,null) : HTMLCollection;
	var align : String;
	var border : String;
	var frame : String;
	var rules : String;
	var summary : String;
	var width : String;
	var bgColor : String;
	var cellPadding : String;
	var cellSpacing : String;
	
	function createCaption() : Element;
	function deleteCaption() : Void;
	function createTHead() : Element;
	function deleteTHead() : Void;
	function createTFoot() : Element;
	function deleteTFoot() : Void;
	function createTBody() : Element;
	/** @throws DOMError */
	function insertRow( ?index : Int = -1 ) : Element;
	/** @throws DOMError */
	function deleteRow( index : Int ) : Void;
}