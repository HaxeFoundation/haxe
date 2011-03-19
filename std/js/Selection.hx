/*
 * Copyright (c) 2005, The haXe Project Contributors
 * All rights reserved.
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 *   - Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *   - Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE HAXE PROJECT CONTRIBUTORS "AS IS" AND ANY
 * EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE HAXE PROJECT CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
 * DAMAGE.
 */
package js;
import js.Dom.Textarea;

class Selection {

	var doc : Dynamic;

	public function new( doc : Textarea ) {
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
