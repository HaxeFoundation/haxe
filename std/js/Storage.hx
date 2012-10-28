/*
 * Copyright (C)2005-2012 Haxe Foundation
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

extern class Storage {

	public function getItem( key : String ) : String;
	public function setItem( key : String, v : String ) : Void;
	public function removeItem( key : String ) : Void;
	public function clear() : Void;

	public var length(default,null) : Int;
	public function key( index : Int ) : String;

	public static inline function getLocal() : Storage {
		var s : Storage;
		try {
			s = untyped window['localStorage'];
			s.getItem('');
		} catch( e : Dynamic ) {
			s = null;
		}
		return s;
	}

	public static inline function getSession() : Storage {
		var s : Storage;
		try {
			s = untyped window['sessionStorage'];
			s.getItem('');
		} catch( e : Dynamic ) {
			s = null;
		}
		return s;
	}

}