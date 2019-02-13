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

// This file is generated from mozilla\HTMLTableCellElement.webidl. Do not edit!

package js.html;

/**
	The `HTMLTableCellElement` interface provides special properties and methods (beyond the regular `HTMLElement` interface it also has available to it by inheritance) for manipulating the layout and presentation of table cells, either header or data cells, in an HTML document.

	Documentation [HTMLTableCellElement](https://developer.mozilla.org/en-US/docs/Web/API/HTMLTableCellElement) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/HTMLTableCellElement$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/HTMLTableCellElement>
**/
@:native("HTMLTableCellElement")
extern class TableCellElement extends Element {
	
	/**
		An unsigned long integer indicating the number of columns this cell must span; this lets the cell occupy space across multiple columns of the table. It reflects the `colspan` attribute.
	**/
	var colSpan : Int;
	
	/**
		An unsigned long integer indicating the number of rows this cell must span; this lets a cell occupy space across multiple rows of the table. It reflects the `rowspan` attribute.
	**/
	var rowSpan : Int;
	
	/**
		Is a `DOMSettableTokenList` describing a list of `id` of `th` elements that represents headers associated with the cell. It reflects the `headers` attribute.
	**/
	var headers : String;
	
	/**
		A long integer representing the cell's position in the `HTMLTableRowElement.cells` collection of the `tr` the cell is contained within. If the cell doesn't belong to a `tr`, it returns `-1`.
	**/
	var cellIndex(default,null) : Int;
	
	/**
		A `DOMString` which can be used on `th` elements (not on `td`), specifying an alternative label for the header cell.. This alternate label can be used in other contexts, such as when describing the headers that apply to a data cell. This is used to offer a shorter term for use by screen readers in particular, and is a valuable accessibility tool. Usually the value of `abbr` is an abbreviation or acronym, but can be any text that's appropriate contextually.
	**/
	var abbr : String;
	
	/**
		
	**/
	var scope : String;
	var align : String;
	var axis : String;
	var height : String;
	var width : String;
	var ch : String;
	var chOff : String;
	var noWrap : Bool;
	var vAlign : String;
	var bgColor : String;
	
}