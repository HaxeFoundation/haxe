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
package haxe.macro;
import haxe.macro.Expr;
import haxe.macro.Context;

/**
	The actual macro implemented for Std.format
**/
class Format {

	#if macro
	public static function format( estr : Expr ) {
		var str = switch( estr.expr ) {
			case EConst(c): switch(c) { case CString(s): s; default: null; }
			default: null;
		};
		if( str == null )
			Context.error("Constant string required", estr.pos);
		var pos = Context.getPosInfos(estr.pos);
		var min = pos.min;
		pos.min++;
		var expr = null;
		function make(size) {
			pos.max = pos.min + size;
			var p = Context.makePosition(pos);
			pos.min += size;
			return p;
		}
		function add(e) {
			if( expr == null )
				expr = e;
			else
				expr = { expr : EBinop(OpAdd,expr,e), pos : Context.makePosition({ min : min, max : pos.min, file : pos.file }) };
		}
		var i = 0, start = 0;
		var max = str.length;
		while( i < max ) {
			if( StringTools.fastCodeAt(str,i++) != '$'.code )
				continue;
			var len = i - start - 1;
			if( len > 0 || expr == null )
				add({ expr : EConst(CString(str.substr(start,len))), pos : make(len) });
			pos.min++;
			start = i;
			var c = StringTools.fastCodeAt(str, i);
			if( c == '{'.code ) {
				var count = 1;
				i++;
				while( i < max ) {
					var c = StringTools.fastCodeAt(str,i++);
					if( c == "}".code ) {
						if( --count == 0 ) break;
					} else if( c == "{".code )
						count++;
				}
				if( count > 0 )
					Context.error("Closing brace not found",make(1));
				pos.min++;
				start++;
				var len = i - start - 1;
				var expr = str.substr(start, len);
				add(Context.parseInlineString(expr, make(len)));
				pos.min++;
				start++;
			} else if( (c >= 'a'.code && c <= 'z'.code) || (c >= 'A'.code && c <= 'Z'.code) || c == '_'.code ) {
				i++;
				while( true ) {
					var c = StringTools.fastCodeAt(str, i);
					if( (c >= 'a'.code && c <= 'z'.code) || (c >= 'A'.code && c <= 'Z'.code) || (c >= '0'.code && c <= '9'.code) || c == '_'.code )
						i++;
					else
						break;
				}
				var len = i - start;
				var ident = str.substr(start, len);
				add( { expr : EConst(CIdent(ident)), pos : make(len) } );
			} else if( c == '$'.code ) {
				start = i++;
				continue;
			} else {
				start = i - 1;
				continue;
			}
			start = i;
		}
		var len = i - start;
		if( len > 0 )
			add({ expr : EConst(CString(str.substr(start,len))), pos : make(len) });
		if( expr == null )
			expr = { expr : EConst(CString("")), pos : make(0) };
		return { expr : ECheckType(expr,TPath({ pack : [], name : "String", params : [] })), pos : expr.pos };
	}
	#end

}
