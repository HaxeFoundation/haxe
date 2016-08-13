/*
 * Copyright (C)2005-2016 Haxe Foundation
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
package php7;


/**
	Various Haxe->PHP compatibility utilities
**/
@:dox(hide)
@:keep
class Boot {

	/**
		Performs `leftExpr >>> rightExpr` operation
	 */
	public static function shiftRightUnsigned( leftExpr:Int, rightExpr:Int ) : Int
	{
		return (rightExpr == 0) ? leftExpr : (leftExpr >= 0) ? (leftExpr >> rightExpr) : (leftExpr >> rightExpr) & (0x7fffffff >> (rightExpr-1))
	}

	/**
		Access properties of untyped things
	 */
	public static function dynamicPropertyAccess( target:Dynamic, field:String ) : Dynamic {
		if (field == 'length' && untyped __call__("is_string", target)) {
			return untyped __call__("strlen", $target)
		} else {
			return __php__("$target->$field");
		}
	}
}


/**
	Implements Haxe's String interface for PHP
 */
class StringImpl
{



}//class StringImpl