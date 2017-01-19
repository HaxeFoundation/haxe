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
package js;

import js.html.TextAreaElement;

class Selection {

	var doc : Dynamic;

	public function new( doc : TextAreaElement ) {
		this.doc = doc;
	}

	public function get() {
		// Mozilla
		if( doc.selectionStart != null )
			return doc.value.substring(doc.selectionStart,doc.selectionEnd);
		// IE
		var range = untyped js.Lib.document.selection.createRange();
		if( range.parentElement() != doc )
			return "";
		return range.text;
	}

	public function select( start : Int, end : Int ) {
		doc.focus();
		// Mozilla
		if( doc.selectionStart != null ) {
			doc.selectionStart = start;
			doc.selectionEnd = end;
			return;
		}
		// FIX : IE count \r\n as one single char for selection
		// operations, we must then deal with it
		var value : String = doc.value;
		var p = 0, delta = 0;
		while( true ) {
			var i = value.indexOf("\r\n", p);
			if( i < 0 || i > start ) break;
			delta++;
			p = i + 2;
		}
		start -= delta;
		while( true ) {
			var i = value.indexOf("\r\n", p);
			if( i < 0 || i > end ) break;
			delta++;
			p = i + 2;
		}
		end -= delta;
		// IE
		var r = doc.createTextRange();
        r.moveEnd('textedit',-1);
        r.moveStart('character',start);
        r.moveEnd('character',end - start);
        r.select();
	}

	public function insert( left : String, text : String, right : String ) {
		doc.focus();
		// Mozilla
		if( doc.selectionStart != null ) {
			var top = doc.scrollTop;
			var start = doc.selectionStart;
			var end = doc.selectionEnd;
			doc.value = doc.value.substr(0,start) + left + text + right + doc.value.substr(end);
			doc.selectionStart = start + left.length;
			doc.selectionEnd = start + left.length + text.length;
			doc.scrollTop = top;
			return;
		}
		// IE
		var range = untyped js.Lib.document.selection.createRange();
		range.text = left + text + right;
		range.moveStart('character',-text.length-right.length);
		range.moveEnd('character',-right.length);
		range.select();
	}

}
