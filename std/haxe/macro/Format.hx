/*
 * Copyright (c) 2005-2011, The haXe Project Contributors
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
				add(Context.parse(expr, make(len)));
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
		return expr;
	}
	#end

}
