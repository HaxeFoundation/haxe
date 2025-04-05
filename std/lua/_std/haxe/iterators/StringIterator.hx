/*
 * Copyright (C)2005-2018 Haxe Foundation
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

package haxe.iterators;
import lua.lib.luautf8.Utf8;

class StringIterator {
    var codes : (String, Int)->StringCodePoint;
    var codepoint : Int;
    var str : String;
    var position : Int;
    public inline function new(s:String) {
        this.codes = Utf8.codes(s);
        this.str = s;
        var cp = codes(str, 0);
        this.codepoint = cp.codepoint;
        this.position = cp.position;
    }

    public inline function hasNext() {
        return codepoint != null;
    }

    public inline function next() {
        var ret = codepoint;
        var cp = codes(str, position);
        codepoint = cp.codepoint;
        position = cp.position;
        return ret;
    }
}
