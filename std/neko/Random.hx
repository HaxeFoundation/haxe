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
package neko;

/**
	A seeded pseudo-random generator.
*/
class Random {

	var r : Dynamic;

	/**
		Create a new random with random seed.
	*/
	public function new() {
		r = random_new();
	}

	/**
		Set the generator seed.
	*/
	public function setSeed( s : Int ) {
		random_set_seed(r,s);
	}

	/**
		Return a random integer modulo max.
	*/
	public function int( max : Int ) : Int {
		return random_int(r,max);
	}

	/**
		Return a random float.
	*/
	public function float() : Float {
		return random_float(r);
	}

	static var random_new = Lib.load("std","random_new",0);
	static var random_set_seed = Lib.load("std","random_set_seed",2);
	static var random_int = Lib.load("std","random_int",2);
	static var random_float = Lib.load("std","random_float",1);
}
