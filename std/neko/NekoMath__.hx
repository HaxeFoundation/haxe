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
package neko;

class NekoMath__
{
	static var pi = Lib.load("std","math_pi",0)();
	static var abs = Lib.load("std","math_abs",1);
	static function min(a,b) { return if( a < b ) a else b; }
	static function max(a,b) { return if( a < b ) b else a; }
	static var sin = Lib.load("std","math_sin",1);
	static var cos = Lib.load("std","math_cos",1);
	static var atan2 = Lib.load("std","math_atan2",2);
	static var tan = Lib.load("std","math_tan",1);
	static var exp = Lib.load("std","math_exp",1);
	static var log = Lib.load("std","math_log",1);
	static var sqrt = Lib.load("std","math_sqrt",1);
	static var round = Lib.load("std","math_round",1);
	static var floor = Lib.load("std","math_floor",1);
	static var ceil = Lib.load("std","math_ceil",1);
	static var atan = Lib.load("std","math_atan",1);
	static var asin = Lib.load("std","math_asin",1);
	static var acos = Lib.load("std","math_acos",1);
	static var pow = Lib.load("std","math_pow",2);

	static var _rnd = Lib.load("std","random_new",0)();
	static var _random = Lib.load("std","random_float",1);
	static function random() { return _random(_rnd); }
}


