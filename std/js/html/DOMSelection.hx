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

/** <p>Selection is the class of the object returned by <code><a rel="custom" href="https://developer.mozilla.org/en/DOM/window.getSelection">window.getSelection()</a></code>
 and other methods. It represents the text selection in the greater page, possibly spanning multiple elements, when the user drags over static text and other parts of the page. For information about text selection in an individual text editing element, see <code><a rel="custom" href="/api/js/html/InputElement">Input</a></code>
, <code><a rel="custom" href="/api/js/html/TextAreaElement">TextArea</a></code>
 and <code><a rel="custom" href="https://developer.mozilla.org/en/DOM/document.activeElement">document.activeElement</a></code>
 which typically return the parent object returned from <code><a rel="custom" href="https://developer.mozilla.org/en/DOM/window.getSelection">window.getSelection()</a></code>
.</p>
<p>A selection object represents the <code><a rel="custom" href="/api/js/html/Range">ranges</a></code>
 that the user has selected. Typically, it holds only one range, accessed as follows:</p>

          <pre name="code" class="js">var selObj = window.getSelection();
var range  = selObj.getRangeAt(0);</pre>
        
<ul> <li><code>selObj</code> is a Selection object</li> <li><code>range</code> is a <a title="en/DOM/Range" rel="internal" href="/api/js/html/Range">Range</a> object</li>
</ul>
<p>Calling the <code><a rel="custom" href="https://developer.mozilla.org/en/DOM/Selection/toString">Selection/toString()</a></code>
 method returns the text contained in the selection, e.g</p>

          <pre name="code" class="js">var selObj = window.getSelection();
window.alert(selObj);</pre>
        
<p>Note that using a selection object as the argument to <code>window.alert</code> will call the object's <code>toString</code> method.</p><br><br>
Documentation for this class was provided by <a href="https://developer.mozilla.org/en/DOM/Selection">MDN</a>. */
@:native("Selection")
extern class DOMSelection
{
	/** Returns the node in which the selection begins. */
	var anchorNode(default,null) : Node;

	/** Returns the number of characters that the selection's anchor is offset within the anchorNode. */
	var anchorOffset(default,null) : Int;

	var baseNode(default,null) : Node;

	var baseOffset(default,null) : Int;

	var extentNode(default,null) : Node;

	var extentOffset(default,null) : Int;

	/** Returns the node in which the selection ends. */
	var focusNode(default,null) : Node;

	/** Returns the number of characters that the selection's focus is offset within the focusNode. */
	var focusOffset(default,null) : Int;

	/** Returns a Boolean indicating whether the selection's start and end points are at the same position. */
	var isCollapsed(default,null) : Bool;

	/** Returns the number of ranges in the selection. */
	var rangeCount(default,null) : Int;

	var type(default,null) : String;

	function addRange( range : Range ) : Void;

	function collapse( node : Node, index : Int ) : Void;

	function collapseToEnd() : Void;

	function collapseToStart() : Void;

	function containsNode( node : Node, allowPartial : Bool ) : Bool;

	function deleteFromDocument() : Void;

	function empty() : Void;

	function extend( node : Node, offset : Int ) : Void;

	function getRangeAt( index : Int ) : Range;

	function modify( alter : String, direction : String, granularity : String ) : Void;

	function removeAllRanges() : Void;

	function selectAllChildren( node : Node ) : Void;

	function setBaseAndExtent( baseNode : Node, baseOffset : Int, extentNode : Node, extentOffset : Int ) : Void;

	function setPosition( node : Node, offset : Int ) : Void;

	function toString() : String;

}
