/*
 * Copyright (C)2005-2013 Haxe Foundation
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

// This file is generated, do not edit!
package js.html;

/** DOM <code>table row</code> objects expose the <code><a class="external" rel="external" href="http://www.w3.org/TR/DOM-Level-2-HTML/html.html#ID-6986576" title="http://www.w3.org/TR/DOM-Level-2-HTML/html.html#ID-6986576" target="_blank">HTMLTableRowElement</a></code> interface, which provides special properties and methods (beyond the regular <a title="en/DOM/element" rel="internal" href="/api/js/html/Element">element</a> object interface they also have available to them by inheritance) for manipulating the layout and presentation of rows in an HTML table.<br><br>
Documentation for this class was provided by <a href="https://developer.mozilla.org/en/DOM/HTMLTableRowElement">MDN</a>. */
@:native("HTMLTableRowElement")
extern class TableRowElement extends Element
{
	/** <a title="en/DOM/tableRow.bgColor" rel="internal" href="https://developer.mozilla.org/en/DOM/tableRow.bgColor" class="new ">row.bgColor</a> 

<span class="deprecatedInlineTemplate" title="">Deprecated</span> */
	var align : String;

	/** row.cells */
	var bgColor : String;

	/** row.ch */
	var cells(default,null) : HTMLCollection;

	/** row.chOff */
	var ch : String;

	/** row.rowIndex */
	var chOff : String;

	/** row.sectionRowIndex */
	var rowIndex(default,null) : Int;

	/** row.vAlign */
	var sectionRowIndex(default,null) : Int;

	var vAlign : String;

	function deleteCell( index : Int ) : Void;

	function insertCell( index : Int ) : Element;

}
