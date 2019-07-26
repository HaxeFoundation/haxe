/*
 * Copyright (C)2005-2019 Haxe Foundation
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

package python.lib;

@:pythonImport("random")
extern class Random {
	/**
		Initialize the random number generator.

		If `a` is omitted or `null`, the current system time is used.
		If randomness sources are provided by the operating system,
		they are used instead of the system time (see the os.urandom()
		function for details on availability).

		If `a` is an int, it is used directly.

		With `version` 2 (the default), a str, bytes, or bytearray object
		gets converted to an int and all of its bits are used.
		With version 1, the hash() of a is used instead.
	**/
	static function seed(?a:Int, ?version:Int):Float;

	/**
		Return an object capturing the current internal state of the generator.
		This object can be passed to setstate() to restore the state.
	**/
	static function getstate():RandomState;

	/**
		`state` should have been obtained from a previous call to `getstate`(),
		and `setstate`() restores the internal state of the generator to what
		it was at the time `getstate`() was called.
	**/
	static function setstate(state:RandomState):Void;

	/**
		Returns a Python integer with `k` random bits.
		This method is supplied with the `MersenneTwister` generator and
		some other generators may also provide it as an optional part of the API.
		When available, `getrandbits`() enables `randrange`() to handle arbitrarily large ranges.
	**/
	static function getrandbits(k:Int):Int;

	/**
		Return a randomly selected element from `range(start, stop, step)`.
		This is equivalent to `choice(range(start, stop, step))`,
		but doesn’t actually build a range object.
	**/
	@:overload(function(stop:Int):Int {})
	static function randrange(start:Int, stop:Int, ?step:Int):Int;

	/**
		Return a random integer N such that `a <= N <= b`. Alias for `randrange(a, b+1)`.
	**/
	static function randint(a:Int, b:Int):Int;

	/**
		Return the next random floating point number in the range [0.0, 1.0).
	**/
	static function random():Float;

	/**
		Return a random floating point number N such that
		`a <= N <= b` for `a <= b` and `b <= N <= a` for `b < a`.
	**/
	static function uniform(a:Float, b:Float):Float;

	/**
		Return a random floating point number N such that
		`low <= N <= high` and with the specified `mode` between those bounds.
		The `low` and `high` bounds default to zero and one.
		The `mode` argument defaults to the midpoint between the bounds,
		giving a symmetric distribution.
	**/
	static function triangular(?low:Float, ?high:Float, ?mode:Float):Float;

	/**
		Beta distribution. Conditions on the parameters are `alpha > 0` and `beta > 0`.
		Returned values range between 0 and 1.
	**/
	static function betavariate(alpha:Float, beta:Float):Float;

	/**
		Exponential distribution. `lambd` is 1.0 divided by the desired mean.
		It should be nonzero. Returned values range from 0 to positive infinity if `lambd` is positive,
		and from negative infinity to 0 if `lambd` is negative.
	**/
	static function expovariate(lambd:Float):Float;

	/**
		Gamma distribution. (Not the gamma function!)
		Conditions on the parameters are `alpha > 0` and `beta > 0`.
	**/
	static function gammavariate(alpha:Float, beta:Float):Float;

	/**
		Gaussian distribution. `mu` is the mean, and `sigma` is the standard deviation.
		This is slightly faster than the `normalvariate` function defined below.
	**/
	static function gauss(mu:Float, sigma:Float):Float;

	/**
		Log normal distribution. If you take the natural logarithm of this distribution,
		you’ll get a normal distribution with mean `mu` and standard deviation `sigma`.
		`mu` can have any value, and `sigma` must be greater than zero.
	**/
	static function lognormvariate(mu:Float, sigma:Float):Float;

	/**
		Normal distribution. `mu` is the mean, and `sigma` is the standard deviation.
	**/
	static function normalvariate(mu:Float, sigma:Float):Float;

	/**
		`mu` is the mean angle, expressed in radians between 0 and 2*pi,
		and `kappa` is the concentration parameter, which must be greater than or equal to zero.
		If `kappa` is equal to zero, this distribution reduces to a uniform random angle
		over the range 0 to 2*pi.
	**/
	static function vonmisesvariate(mu:Float, kappa:Float):Float;

	/**
		Pareto distribution. alpha is the `shape` parameter.
	**/
	static function paretovariate(alpha:Float):Float;

	/**
		Weibull distribution. `alpha` is the scale parameter and `beta` is the shape parameter.
	**/
	static function weibullvariate(alpha:Float, beta:Float):Float;
}

abstract RandomState({}) {}
