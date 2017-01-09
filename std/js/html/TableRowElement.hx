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

// This file is generated from mozilla\HTMLTableRowElement.webidl. Do not edit!

package js.html;

/**
	The `HTMLTableRowElement` interface provides special properties and methods (beyond the `HTMLElement` interface it also has available to it by inheritance) for manipulating the layout and presentation of rows in an HTML table.

	Documentation [HTMLTableRowElement](https://developer.mozilla.org/en-US/docs/Web/API/HTMLTableRowElement) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/HTMLTableRowElement$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/HTMLTableRowElement>
**/
@:native("HTMLTableRowElement")
extern class TableRowElement extends Element
{
	
	/**
		Returns a `long` value which gives the logical position of the row within the entire table. If the row is not part of a table, returns `-1`.
	**/
	var rowIndex(default,null) : Int;
	
	/**
		Returns a `long` value which gives the logical position of the row within the table section it belongs to. If the row is not part of a section, returns `-1`.
	**/
	var sectionRowIndex(default,null) : Int;
	
	/**
		Returns a live `HTMLCollection` containing the cells in the row. The `HTMLCollection` is live and is automatically updated when cells are added or removed.
	**/
	var cells(default,null) : HTMLCollection;
	
	/**
		Is a `DOMString` containing an enumerated value reflecting the `align` attribute. It indicates the alignment of the element's contents with respect to the surrounding context. The possible values are `"left"`, `"right"`, and `"center"`.
	**/
	var align : String;
	
	/**
		Is a `DOMString` containing one single character. This character is the one to align all the cell of a column on. It reflects the `char` and default to the decimal points associated with the language, e.g. `'.'` for English, or `','` for French. This property was optional and was not very well supported.
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
	
	/**
		Is a `DOMString` containing the background color of the cells. It reflects the obsolete `bgColor` attribute.
	**/
	var bgColor : String;
	
	/** @throws DOMError */
	
	/**
		Inserts a new cell just before the given position in the row. If the given position is not given or is `-1`, it appends the cell to the row. If the given position is greater (or equal as it starts at zero) than the amount of cells in the row, or is smaller than `-1`, it raises a `DOMException` with the `IndexSizeError` value. Returns a reference to a HTMLTableCellElement [en-US].
	**/
	function insertCell( ?index : Int = -1 ) : Element;
	/** @throws DOMError */
	
	/**
		Removes the cell at the given position in the row. If the given position is greater (or equal as it starts at zero) than the amount of cells in the row, or is smaller than `0`, it raises a `DOMException` with the `IndexSizeError` value.
	**/
	function deleteCell( index : Int ) : Void;
}