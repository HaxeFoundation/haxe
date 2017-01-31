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

// This file is generated from mozilla\HTMLTableCellElement.webidl. Do not edit!

package js.html;

/**
	The `HTMLTableCellElement` interface provides special properties and methods (beyond the regular `HTMLElement` interface it also has available to it by inheritance) for manipulating the layout and presentation of table cells, either header or data cells, in an HTML document.

	Documentation [HTMLTableCellElement](https://developer.mozilla.org/en-US/docs/Web/API/HTMLTableCellElement) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/HTMLTableCellElement$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/HTMLTableCellElement>
**/
@:native("HTMLTableCellElement")
extern class TableCellElement extends Element
{
	
	/**
		Is a `unsigned` `long` that represents the number of columns this cell must span. It reflects the `colspan` attribute.
	**/
	var colSpan : Int;
	
	/**
		Is a `unsigned` `long` that represents the number of rows this cell must span. It reflects the `rowspan` attribute.
	**/
	var rowSpan : Int;
	
	/**
		Is a `DOMSettableTokenList` describing a list of `id` of `th` elements that represents headers associated with the cell. It reflects the `headers` attribute.
	**/
	var headers : String;
	
	/**
		Is a `long` representing the cell position in the cells collection of the `tr` it belongs to. If the cell doesn't belong to a `tr`, it returns `-1`.
	**/
	var cellIndex(default,null) : Int;
	var abbr : String;
	var scope : String;
	
	/**
		Is a `DOMString` containing an enumerated value reflecting the `align` attribute. It indicates the alignment of the element's contents with respect to the surrounding context. The possible values are `"left"`, `"right"`, and `"center"`.
	**/
	var align : String;
	
	/**
		Is a `DOMString` containing a name grouping cells in virtual. It reflects the obsolete `axis` attribute.
	**/
	var axis : String;
	
	/**
		Is a `DOMString` containing a length of pixel of the hinted height of the cell. It reflects the obsolete `height` attribute.
	**/
	var height : String;
	
	/**
		Is a `DOMString` containing a length of pixel of the hinted width of the cell. It reflects the obsolete `width` attribute.
	**/
	var width : String;
	
	/**
		Is a `DOMString` containing one single chararcter. This character is the one to align all the cell of a column on. It reflects the `char` and default to the decimal points associated with the language, e.g. `'.'` for English, or `','` for French. This property was optional and was not very well supported.
	**/
	var ch : String;
	
	/**
		Is a `DOMString` containing a integer indicating how many characters must be left at the right (for left-to-right scripts; or at the left for right-to-left scripts) of the character defined by `HTMLTableCellElement.ch`. This property was optional and was not very well supported.
	**/
	var chOff : String;
	
	/**
		Is a `Boolean` value reflecting the `nowrap` attribute and indicating if cell content can be broken in several lines.
	**/
	var noWrap : Bool;
	
	/**
		Is a `DOMString` representing an enumerated value indicating how the content of the cell must be vertically aligned. It reflects the `valign` attribute and can have one of the following values: `"top"`, `"middle"`, `"bottom"`, or `"baseline"`.
	**/
	var vAlign : String;
	
	/**
		Is a `DOMString` containing the background color of the cells. It reflects the obsolete `bgColor` attribute.
	**/
	var bgColor : String;
	
}