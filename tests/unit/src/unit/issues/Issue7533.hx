package unit.issues;

#if php
import php.*;
#end

class Issue7533 extends unit.Test {
	#if php
	function test() {
		var signBit = Const.PHP_INT_SIZE * 8 - 1; //31 or 63

		var bint:Int = Syntax.code('bindec(decbin({0}))', 1 << signBit);
		/**
		 * `bindec(decbin())` is to convert `1 << signBit` to a positive float instead of a negative int.
		 * This is the only way for it to fall into `else if(left >= 0)` branch of `php.Boot.shiftRightUnsigned()`.
		 * And then on a bitwise operation it will be converted back to a negative integer by PHP interpreter.
		 * And that causes this bug.
		 */

		eq( 1 << (signBit - 2), bint >>> 2 );
	}
	#end
}