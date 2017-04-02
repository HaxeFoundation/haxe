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

class PositionTools {

	/**
		Returns the `Position` where the caller of `here` is.
	**/
	macro public static function here():ExprOf<Position> {
		var positionExpr = Context.makeExpr(Context.getPosInfos(Context.currentPos()), Context.currentPos());
		if (Context.defined("macro")) {
			return macro haxe.macro.Context.makePosition($positionExpr);
		} else {
			return positionExpr;
		}
	}

	/**
		Like `Context.getPosInfos`, except this method is available on all platforms.
	**/
	public static function getInfos( p : Position ) : { min : Int, max : Int, file : String } {
		#if macro
		return Context.getPosInfos(p);
		#else
		return p;
		#end
	}

	/**
		Like `Context.makePosition`, except this method is available on all platforms.
	**/
	public static function make( inf : { min : Int, max : Int, file : String } ) : Position {
		#if macro
		return Context.makePosition(inf);
		#else
		return inf;
		#end
	}

}
