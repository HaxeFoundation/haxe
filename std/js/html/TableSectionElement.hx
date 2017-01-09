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

// This file is generated from mozilla\HTMLTableSectionElement.webidl. Do not edit!

package js.html;

/**
	The `HTMLTableSectionElement` interface provides special properties and methods (beyond the `HTMLElement` interface it also has available to it by inheritance) for manipulating the layout and presentation of sections, that is headers, footers and bodies, in an HTML table.

	Documentation [HTMLTableSectionElement](https://developer.mozilla.org/en-US/docs/Web/API/HTMLTableSectionElement) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/HTMLTableSectionElement$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/HTMLTableSectionElement>
**/
@:native("HTMLTableSectionElement")
extern class TableSectionElement extends Element
{
	
	/**
		Returns a live `HTMLCollection` containing the rows in the section. The `HTMLCollection` is live and is automatically updated when rows are added or removed.
	**/
	var rows(default,null) : HTMLCollection;
	
	/**
		Is a `DOMString` containing an enumerated value reflecting the `align` attribute. It indicates the alignment of the element's contents with respect to the surrounding context. The possible values are `"left"`, `"right"`, and `"center"`.
	**/
	var align : String;
	
	/**
		Is a `DOMString` containing one single chararcter. This character is the one to align all the cell of a column on. It reflects the `char` and default to the decimal points associated with the language, e.g. `'.'` for English, or `','` for French. This property was optional and was not very well supported.
	**/
	var ch : String;
	
	/**
		Is a `DOMString` containing a integer indicating how many characters must be left at the right (for left-to-right scripts; or at the left for right-to-left scripts) of the character defined by `HTMLTableRowElement.ch`. This property was optional and was not very well supported.
	**/
	var chOff : String;
	
	/**
		Is a `DOMString` representing an enumerated value indicating how the content of the cell must be vertically aligned. It reflects the `valign` attribute and can have one of the following values: `"top"`, `"middle"`, `"bottom"`, or `"baseline"`.
	**/
	var vAlign : String;
	
	/** @throws DOMError */
	
	/**
		Inserts a new row just before the given position in the section. If the given position is not given or is `-1`, it appends the row to the end of section. If the given position is greater (or equal as it starts at zero) than the amount of rows in the section, or is smaller than `-1`, it raises a `DOMException` with the `IndexSizeError` value.
	**/
	function insertRow( ?index : Int = -1 ) : Element;
	/** @throws DOMError */
	
	/**
		Removes the cell at the given position in the section. If the given position is greater (or equal as it starts at zero) than the amount of rows in the section, or is smaller than `0`, it raises a `DOMException` with the `IndexSizeError` value.
	**/
	function deleteRow( index : Int ) : Void;
}