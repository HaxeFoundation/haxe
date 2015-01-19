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

// This file is generated from mozilla/HTMLElement.webidl line 15:0. Do not edit!

package js.html;

@:native("HTMLElement")
extern class HTMLElement extends Element
{
	var title : String;
	var lang : String;
	var dir : String;
	var dataset(default,null) : DOMStringMap;
	var itemScope : Bool;
	var itemType(default,null) : DOMSettableTokenList;
	var itemId : String;
	var itemRef(default,null) : DOMSettableTokenList;
	var itemProp(default,null) : DOMSettableTokenList;
	var properties(default,null) : HTMLPropertiesCollection;
	var itemValue : Dynamic;
	var hidden : Bool;
	var tabIndex : Int;
	var accessKey : String;
	var accessKeyLabel(default,null) : String;
	var draggable : Bool;
	var contentEditable : String;
	var isContentEditable(default,null) : Bool;
	var contextMenu(default,null) : MenuElement;
	var spellcheck : Bool;
	var offsetParent(default,null) : Element;
	var offsetTop(default,null) : Int;
	var offsetLeft(default,null) : Int;
	var offsetWidth(default,null) : Int;
	var offsetHeight(default,null) : Int;
	
	function click() : Void;
	/** @throws DOMError */
	function focus() : Void;
	/** @throws DOMError */
	function blur() : Void;
}