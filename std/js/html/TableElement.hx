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

/** <code>table</code> objects expose the <code><a class="external" rel="external" href="http://www.w3.org/TR/DOM-Level-2-HTML/html.html#ID-64060425" title="http://www.w3.org/TR/DOM-Level-2-HTML/html.html#ID-64060425" target="_blank">HTMLTableElement</a></code> interface, which provides special properties and methods (beyond the regular <a rel="internal" href="https://developer.mozilla.org/en/DOM/element" title="en/DOM/element">element</a> object interface they also have available to them by inheritance) for manipulating the layout and presentation of tables in HTML.
<br><br>
Documentation for this class was provided by <a href="https://developer.mozilla.org/en/DOM/HTMLTableElement">MDN</a>. */
@:native("HTMLTableElement")
extern class TableElement extends Element
{
	/** <b>align</b> gets/sets the alignment of the table. */
	var align : String;

	/** <b>bgColor</b> gets/sets the background color of the table. */
	var bgColor : String;

	/** <b>border</b> gets/sets the table border. */
	var border : String;

	/** <b>caption</b> returns the table caption.
 Setter throws DOMException. */
	var caption : TableCaptionElement;

	/** <b>cellPadding</b> gets/sets the cell padding. */
	var cellPadding : String;

	/** <b>cellSpacing</b> gets/sets the spacing around the table. */
	var cellSpacing : String;

	/** <b>frame</b> specifies which sides of the table have borders. */
	var frame : String;

	/** <b>rows</b> returns the rows in the table. */
	var rows (default,null) : HTMLCollection;

	/** <b>rules</b> specifies which interior borders are visible. */
	var rules : String;

	/** <b>summary</b> gets/sets the table summary. */
	var summary : String;

	/** <b>tBodies</b> returns the table bodies. */
	var tBodies (default,null) : HTMLCollection;

	/** <b>tFoot</b> returns the table footer.
 Setter throws DOMException. */
	var tFoot : TableSectionElement;

	/** <b>tHead</b> returns the table head.
 Setter throws DOMException. */
	var tHead : TableSectionElement;

	/** <b>width</b> gets/sets the width of the table. */
	var width : String;

	function createCaption() : Element;

	function createTBody() : Element;

	function createTFoot() : Element;

	function createTHead() : Element;

	function deleteCaption() : Void;

	function deleteRow( index : Int ) : Void;

	function deleteTFoot() : Void;

	function deleteTHead() : Void;

	function insertRow( index : Int ) : Element;

}
