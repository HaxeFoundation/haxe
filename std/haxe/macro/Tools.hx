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
package haxe.macro;
#if macro
import haxe.macro.Expr;
#end

/**
	Some macro utility methods that can be used on different platforms
**/
#if !macro extern #end
class Tools {

	#if (js || macro)
	/**
		Embed an on-disk javascript file (can be called into an __init__ method)
	**/
	@:macro public static function includeFile( fileName : Expr ) {
		var str = switch( fileName.expr ) {
		case EConst(c):
			switch( c ) {
			case CString(str): str;
			default: null;
			}
		default: null;
		}
		if( str == null ) Context.error("Should be a constant string", fileName.pos);
		var f = try sys.io.File.getContent(Context.resolvePath(str)) catch( e : Dynamic ) Context.error(Std.string(e), fileName.pos);
		var p = Context.currentPos();
		return { expr : EUntyped( { expr : ECall( { expr : EConst(CIdent("__js__")), pos : p }, [ { expr : EConst(CString(f)), pos : p } ]), pos : p } ), pos : p };
	}
	#end

}