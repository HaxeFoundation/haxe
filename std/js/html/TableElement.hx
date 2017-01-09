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

// This file is generated from mozilla\HTMLTableElement.webidl. Do not edit!

package js.html;

/**
	The `HTMLTableElement` interface provides special properties and methods (beyond the regular `HTMLElement` object interface it also has available to it by inheritance) for manipulating the layout and presentation of tables in an HTML document.

	Documentation [HTMLTableElement](https://developer.mozilla.org/en-US/docs/Web/API/HTMLTableElement) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/HTMLTableElement$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/HTMLTableElement>
**/
@:native("HTMLTableElement")
extern class TableElement extends Element
{
	
	/**
		Is a `HTMLTableCaptionElement` representing the first `caption` that is a child of the element, or `null` if none is found. When set, if the object doesn't represent a `caption`, a `DOMException` with the `HierarchyRequestError` name is thrown. If a correct object is given, it is inserted in the tree as the first child of this element and the first `caption` that is a child of this element is removed from the tree, if any.
	**/
	var caption : TableCaptionElement;
	
	/**
		Is a `HTMLTableSectionElement` representing the first `thead` that is a child of the element, or `null` if none is found. When set, if the object doesn't represent a `thead`, a `DOMException` with the `HierarchyRequestError` name is thrown. If a correct object is given, it is inserted in the tree immediately before the first element that is neither a `caption`, nor a `colgroup`, or as the last child if there is no such element, and the first `thead` that is a child of this element is removed from the tree, if any.
	**/
	var tHead : TableSectionElement;
	
	/**
		Is a `HTMLTableSectionElement` representing the first `tfoot` that is a child of the element, or `null` if none is found. When set, if the object doesn't represent a `tfoot`, a `DOMException` with the `HierarchyRequestError` name is thrown. If a correct object is given, it is inserted in the tree immediately before the first element that is neither a `caption`, a `colgroup`, nor a `thead`, or as the last child if there is no such element, and the first `tfoot` that is a child of this element is removed from the tree, if any.
	**/
	var tFoot : TableSectionElement;
	
	/**
		Returns a live `HTMLCollection` containing all the `tbody` of the element. The `HTMLCollection` is live and is automatically updated when the `HTMLTableElement` changes.
	**/
	var tBodies(default,null) : HTMLCollection;
	
	/**
		Returns a live `HTMLCollection` containing all the rows of the element, that is all `tr` that are a child of the element, or a child or one of its `thead`, `tbody` and `tfoot` children. The rows members of a `thead` appear first, in tree order, and those members of a `tbody` last, also in tree order. The `HTMLCollection` is live and is automatically updated when the `HTMLTableElement` changes.
	**/
	var rows(default,null) : HTMLCollection;
	
	/**
		Is a `DOMString` containing an enumerated value reflecting the `align` attribute. It indicates the alignment of the element's contents with respect to the surrounding context. The possible values are `"left"`, `"right"`, and `"center"`.
	**/
	var align : String;
	
	/**
		Is a `DOMString` containing the width in pixels of the border of the table. It reflects the obsolete `border` attribute.
	**/
	var border : String;
	
	/**
		Is a `DOMString` containing the type of the external borders of the table. It reflects the obsolete `frame` attribute and can take one of the following values: `"void"`, `"above"`, `"below"`, `"hsides"`, `"vsides"`, `"lhs"`, `"rhs"`, `"box"`, or `"border"`.
	**/
	var frame : String;
	
	/**
		Is a `DOMString` containing the type of the internal borders of the table. It reflects the obsolete `rules` attribute and can take one of the following values: `"none"`, `"groups"`, `"rows"`, `"cols"`, or `"all"`.
	**/
	var rules : String;
	
	/**
		Is a `DOMString` containing a description of the purpose or the structure of the table. It reflects the obsolete `summary` attribute.
	**/
	var summary : String;
	
	/**
		Is a `DOMString` containing the length in pixels or in percentage of the desired width fo the entire table. It reflects the obsolete `width` attribute.
	**/
	var width : String;
	
	/**
		Is a `DOMString` containing the background color of the cells. It reflects the obsolete `bgColor` attribute.
	**/
	var bgColor : String;
	
	/**
		Is a `DOMString` containing the width in pixels of the horizontal and vertical sapce between cell content and cell borders. It reflects the obsolete `cellpadding` attribute.
	**/
	var cellPadding : String;
	
	/**
		Is a `DOMString` containing the width in pixels of the horizontal and vertical separation between cells. It reflects the obsolete `cellspacing` attribute.
	**/
	var cellSpacing : String;
	
	
	/**
		Returns an `HTMLElement` representing the first `caption` that is a child of the element. If none is found, a new one is created and inserted in the tree as the first child of the `table` element.
	**/
	function createCaption() : Element;
	
	/**
		Removes the first `caption` that is a child of the element.
	**/
	function deleteCaption() : Void;
	
	/**
		Returns an `HTMLElement` representing the first `thead` that is a child of the element. If none is found, a new one is created and inserted in the tree immediately before the first element that is neither a `caption`, nor a `colgroup`, or as the last child if there is no such element.
	**/
	function createTHead() : Element;
	
	/**
		Removes the first `thead` that is a child of the element.
	**/
	function deleteTHead() : Void;
	
	/**
		Returns an `HTMLElement` representing the first `tfoot` that is a child of the element. If none is found, a new one is created and inserted in the tree immediately before the first element that is neither a `caption`, a `colgroup`, nor a `thead`, or as the last child if there is no such element.
	**/
	function createTFoot() : Element;
	
	/**
		Removes the first `tfoot` that is a child of the element.
	**/
	function deleteTFoot() : Void;
	function createTBody() : Element;
	/** @throws DOMError */
	
	/**
		Returns an `HTMLTableRowElement` representing a new row of the table. It inserts it in the rows collection immediately before the `tr` element at the given `index` position. If necessary a `tbody` is created. If the `index` is `-1`, the new row is appended to the collection. If the `index` is smaller than `-1` or greater than the number of rows in the collection, a `DOMException` with the value `IndexSizeError` is raised.
	**/
	function insertRow( ?index : Int = -1 ) : Element;
	/** @throws DOMError */
	
	/**
		Removes the row corresponding to the `index` given in parameter. If the `index` value is `-1` the last row is removed; if it smaller than `-1` or greater than the amount of rows in the collection, a `DOMException` with the value `IndexSizeError` is raised.
	**/
	function deleteRow( index : Int ) : Void;
}