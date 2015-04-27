// constants
var zero = 0.0;
var one = 1.0;
//1.0 / zero == Math.POSITIVE_INFINITY;
//-1.0 / zero == Math.NEGATIVE_INFINITY;
(Math.NaN == Math.NaN) == false;
Math.isNaN(Math.NaN) == true;
Math.isNaN(Math.sqrt( -1)) == true;
Math.NEGATIVE_INFINITY == Math.NEGATIVE_INFINITY;
Math.POSITIVE_INFINITY == Math.POSITIVE_INFINITY;
// +
Math.POSITIVE_INFINITY + Math.POSITIVE_INFINITY == Math.POSITIVE_INFINITY;
Math.NEGATIVE_INFINITY + Math.NEGATIVE_INFINITY == Math.NEGATIVE_INFINITY;
Math.POSITIVE_INFINITY + one == Math.POSITIVE_INFINITY;
Math.NEGATIVE_INFINITY + one == Math.NEGATIVE_INFINITY;
Math.isNaN(Math.POSITIVE_INFINITY + Math.NEGATIVE_INFINITY) == true;
Math.isNaN(Math.POSITIVE_INFINITY + Math.NaN) == true;
Math.isNaN(Math.NEGATIVE_INFINITY + Math.NaN) == true;
// -
one - Math.POSITIVE_INFINITY == Math.NEGATIVE_INFINITY;
one - Math.NEGATIVE_INFINITY == Math.POSITIVE_INFINITY;
-Math.POSITIVE_INFINITY == Math.NEGATIVE_INFINITY;
-Math.NEGATIVE_INFINITY == Math.POSITIVE_INFINITY;
Math.POSITIVE_INFINITY - one == Math.POSITIVE_INFINITY;
Math.NEGATIVE_INFINITY - one == Math.NEGATIVE_INFINITY;
Math.isNaN(Math.POSITIVE_INFINITY - Math.POSITIVE_INFINITY ) == true;
Math.isNaN(Math.NEGATIVE_INFINITY - Math.NEGATIVE_INFINITY) == true;
Math.POSITIVE_INFINITY - Math.NEGATIVE_INFINITY == Math.POSITIVE_INFINITY;
Math.NEGATIVE_INFINITY - Math.POSITIVE_INFINITY == Math.NEGATIVE_INFINITY;
Math.isNaN(Math.POSITIVE_INFINITY - Math.NaN) == true;
Math.isNaN(Math.NEGATIVE_INFINITY - Math.NaN) == true;
Math.isNaN(Math.NaN - Math.POSITIVE_INFINITY) == true;
Math.isNaN(Math.NaN - Math.NEGATIVE_INFINITY) == true;
// *
Math.POSITIVE_INFINITY * one == Math.POSITIVE_INFINITY;
Math.NEGATIVE_INFINITY * one == Math.NEGATIVE_INFINITY;
Math.isNaN(Math.POSITIVE_INFINITY * zero) == true;
Math.isNaN(Math.NEGATIVE_INFINITY * zero) == true;
Math.POSITIVE_INFINITY * Math.POSITIVE_INFINITY == Math.POSITIVE_INFINITY;
Math.NEGATIVE_INFINITY * Math.NEGATIVE_INFINITY  == Math.POSITIVE_INFINITY;
Math.POSITIVE_INFINITY * Math.NEGATIVE_INFINITY == Math.NEGATIVE_INFINITY;
Math.isNaN(Math.POSITIVE_INFINITY * Math.NaN) == true;
Math.isNaN(Math.NEGATIVE_INFINITY * Math.NaN) == true;
// /
Math.POSITIVE_INFINITY / one == Math.POSITIVE_INFINITY;
Math.NEGATIVE_INFINITY / one == Math.NEGATIVE_INFINITY;
//Math.POSITIVE_INFINITY / zero == Math.POSITIVE_INFINITY;
//Math.NEGATIVE_INFINITY / zero == Math.NEGATIVE_INFINITY;
Math.isNaN(Math.POSITIVE_INFINITY / Math.POSITIVE_INFINITY);
Math.isNaN(Math.POSITIVE_INFINITY / Math.NEGATIVE_INFINITY);
Math.isNaN(Math.NEGATIVE_INFINITY / Math.POSITIVE_INFINITY);
Math.isNaN(Math.NEGATIVE_INFINITY / Math.NEGATIVE_INFINITY);
Math.isNaN(Math.NaN / Math.POSITIVE_INFINITY);
Math.isNaN(Math.POSITIVE_INFINITY / Math.NaN);
Math.isNaN(Math.NaN / Math.POSITIVE_INFINITY);
Math.isNaN(Math.NEGATIVE_INFINITY / Math.NaN);

// abs
Math.abs(-1.223) == 1.223;
Math.abs(1.223) == 1.223;
Math.abs(0) == 0;
Math.isNaN(Math.abs(Math.NaN)) == true;
Math.abs(Math.NEGATIVE_INFINITY) == Math.POSITIVE_INFINITY;
Math.abs(Math.POSITIVE_INFINITY) == Math.POSITIVE_INFINITY;

// min
Math.min(0.0, 1.0) == 0.0;
Math.min(0.0, -1.0) == -1.0;
Math.min(0.0, 0.0) == 0.0;
Math.min(1.0, 1.0) == 1.0;
Math.min(Math.NEGATIVE_INFINITY, Math.NEGATIVE_INFINITY) == Math.NEGATIVE_INFINITY;
Math.min(Math.NEGATIVE_INFINITY, Math.POSITIVE_INFINITY) == Math.NEGATIVE_INFINITY;
Math.min(Math.POSITIVE_INFINITY, Math.POSITIVE_INFINITY) == Math.POSITIVE_INFINITY;
Math.min(Math.POSITIVE_INFINITY, zero) == zero;
Math.min(Math.NEGATIVE_INFINITY, zero) == Math.NEGATIVE_INFINITY;
Math.isNaN(Math.min(Math.NEGATIVE_INFINITY, Math.NaN)) == true;
Math.isNaN(Math.min(Math.POSITIVE_INFINITY, Math.NaN)) == true;
Math.isNaN(Math.min(Math.NaN, Math.NaN)) == true;
Math.isNaN(Math.min(one, Math.NaN)) == true;
Math.isNaN(Math.min(zero, Math.NaN)) == true;
Math.isNaN(Math.min(Math.NaN, Math.NEGATIVE_INFINITY)) == true;
Math.isNaN(Math.min(Math.NaN,Math.POSITIVE_INFINITY)) == true;
Math.isNaN(Math.min(Math.NaN, one)) == true;
Math.isNaN(Math.min(Math.NaN, zero)) == true;

// max
Math.max(0.0, 1.0) == 1.0;
Math.max(0.0, -1.0) == 0.0;
Math.max(0.0, 0.0) == 0.0;
Math.max(1.0, 1.0) == 1.0;
Math.max(Math.NEGATIVE_INFINITY, Math.NEGATIVE_INFINITY) == Math.NEGATIVE_INFINITY;
Math.max(Math.NEGATIVE_INFINITY, Math.POSITIVE_INFINITY) == Math.POSITIVE_INFINITY;
Math.max(Math.POSITIVE_INFINITY, Math.POSITIVE_INFINITY) == Math.POSITIVE_INFINITY;
Math.max(Math.POSITIVE_INFINITY, zero) == Math.POSITIVE_INFINITY;
Math.max(Math.NEGATIVE_INFINITY, zero) == 0;
Math.isNaN(Math.max(Math.NEGATIVE_INFINITY, Math.NaN)) == true;
Math.isNaN(Math.max(Math.POSITIVE_INFINITY, Math.NaN)) == true;
Math.isNaN(Math.max(Math.NaN, Math.NaN)) == true;
Math.isNaN(Math.max(one, Math.NaN)) == true;
Math.isNaN(Math.max(zero, Math.NaN)) == true;
Math.isNaN(Math.max(Math.NaN, Math.NEGATIVE_INFINITY)) == true;
Math.isNaN(Math.max(Math.NaN,Math.POSITIVE_INFINITY)) == true;
Math.isNaN(Math.max(Math.NaN, one)) == true;
Math.isNaN(Math.max(Math.NaN, zero)) == true;

// sin
Math.sin(0.0) == 0.0;
Math.sin(Math.PI / 2) == 1.0;
Math.sin(Math.PI) == 0.0;
Math.sin(Math.PI * 3 / 2) == -1.0;
Math.isNaN(Math.sin(Math.POSITIVE_INFINITY)) == true;
Math.isNaN(Math.sin(Math.NEGATIVE_INFINITY)) == true;
Math.isNaN(Math.sin(Math.NaN)) == true;

// cos
Math.cos(0.0) == 1.0;
Math.cos(Math.PI / 2) == 0.0;
Math.cos(Math.PI) == -1.0;
Math.cos(Math.PI * 3 / 2) == 0.0;
Math.isNaN(Math.cos(Math.POSITIVE_INFINITY)) == true;
Math.isNaN(Math.cos(Math.NEGATIVE_INFINITY)) == true;
Math.isNaN(Math.cos(Math.NaN)) == true;

// exp
Math.exp(0.0) == 1.0;
Math.exp(1.0) == 2.7182818284590452353602874713527;
Math.exp(Math.POSITIVE_INFINITY) == Math.POSITIVE_INFINITY;
Math.exp(Math.NEGATIVE_INFINITY) == 0.0;
Math.isNaN(Math.exp(Math.NaN)) == true;

// log
Math.log(0.0) == Math.NEGATIVE_INFINITY;
Math.log(2.7182818284590452353602874713527) == 1.0;
Math.isNaN(Math.log( -1.0)) == true;
Math.isNaN(Math.log(Math.NaN)) == true;
Math.isNaN(Math.log(Math.NEGATIVE_INFINITY)) == true;
Math.log(Math.POSITIVE_INFINITY) == Math.POSITIVE_INFINITY;

// exp + log
var floats = [1.33, 12.0, -112.999992, 0.0, Math.NEGATIVE_INFINITY, Math.POSITIVE_INFINITY];
for (f in floats) {
	#if !php
	feq(Math.log(Math.exp(f)), f);
	#end
}

// sqrt
Math.sqrt(4.0) == 2;
Math.sqrt(0.0) == 0.0;
Math.sqrt(Math.POSITIVE_INFINITY) == Math.POSITIVE_INFINITY;
Math.isNaN(Math.sqrt(Math.NEGATIVE_INFINITY)) == true;
Math.isNaN(Math.sqrt(Math.NaN)) == true;
Math.isNaN(Math.sqrt( -1.0)) == true;

// round
Math.round(0.0) == 0;
Math.round(0.1) == 0;
Math.round(0.4999) == 0;
Math.round(0.5) == 1;
Math.round(1.0) == 1;
Math.round(1.499) == 1;
Math.round(-0.1) == 0;
Math.round(-0.4999) == 0;
Math.round(-0.5) == 0;
Math.round(-0.50001) == -1;
Math.round(-1.0) == -1;
Math.round(-1.499) == -1;
Math.round(-1.5) == -1;
Math.round( -1.50001) == -2;
Math.fround(Math.POSITIVE_INFINITY) == Math.POSITIVE_INFINITY;
Math.fround(Math.NEGATIVE_INFINITY) == Math.NEGATIVE_INFINITY;
Math.isNaN(Math.fround(Math.NaN)) == true;
Math.fround(0.0) == 0.0;
Math.fround(0.1) == 0.0;
Math.fround(0.4999) == 0.0;
Math.fround(0.5) == 1.0;
Math.fround(1.0) == 1.0;
Math.fround(1.499) == 1.0;
Math.fround(1.5) == 2.0;
Math.fround(-0.1) == -0.0;
Math.fround(-0.4999) == -0.0;
Math.fround(-0.5) == -0.0;
Math.fround(-0.50001) == -1.0;
Math.fround(-1.0) == -1.0;
Math.fround(-1.499) == -1.0;
Math.fround(-1.5) == -1.0;
Math.fround( -1.50001) == -2.0;

// floor
Math.floor(0.0) == 0;
Math.floor(0.9999) == 0;
Math.floor(1.0) == 1;
Math.floor( -0.0001) == -1;
Math.floor( -1.0) == -1;
Math.floor( -1.0001) == -2;
Math.ffloor(Math.POSITIVE_INFINITY) == Math.POSITIVE_INFINITY;
Math.ffloor(Math.NEGATIVE_INFINITY) == Math.NEGATIVE_INFINITY;
Math.isNaN(Math.ffloor(Math.NaN)) == true;

// ceil
Math.ceil(0.0) == 0;
Math.ceil(-0.9999) == 0;
Math.ceil(-1.0) == -1;
Math.ceil( 0.0001) == 1;
Math.ceil( 1.0) == 1;
Math.ceil( 1.0001) == 2;
Math.fceil(Math.POSITIVE_INFINITY) == Math.POSITIVE_INFINITY;
Math.fceil(Math.NEGATIVE_INFINITY) == Math.NEGATIVE_INFINITY;
Math.isNaN(Math.fceil(Math.NaN)) == true;

// random
// not much to test here...

// isFinite
Math.isFinite(Math.POSITIVE_INFINITY) == false;
Math.isFinite(Math.NEGATIVE_INFINITY) == false;
Math.isFinite(Math.NaN) == false;
Math.isFinite(0.0) == true;

// isNaN
Math.isNaN(Math.POSITIVE_INFINITY) == false;
Math.isNaN(Math.NEGATIVE_INFINITY) == false;
Math.isNaN(Math.NaN) == true;
Math.isNaN(0.0) == false;


// Dynamic version
var math = Math;


//1.0 / zero == math.POSITIVE_INFINITY;
//-1.0 / zero == math.NEGATIVE_INFINITY;
(math.NaN == math.NaN) == false;
math.isNaN(math.NaN) == true;
math.isNaN(math.sqrt( -1)) == true;
math.NEGATIVE_INFINITY == math.NEGATIVE_INFINITY;
math.POSITIVE_INFINITY == math.POSITIVE_INFINITY;
// +
math.POSITIVE_INFINITY + math.POSITIVE_INFINITY == math.POSITIVE_INFINITY;
math.NEGATIVE_INFINITY + math.NEGATIVE_INFINITY == math.NEGATIVE_INFINITY;
math.POSITIVE_INFINITY + one == math.POSITIVE_INFINITY;
math.NEGATIVE_INFINITY + one == math.NEGATIVE_INFINITY;
math.isNaN(math.POSITIVE_INFINITY + math.NEGATIVE_INFINITY) == true;
math.isNaN(math.POSITIVE_INFINITY + math.NaN) == true;
math.isNaN(math.NEGATIVE_INFINITY + math.NaN) == true;
// -
one - math.POSITIVE_INFINITY == math.NEGATIVE_INFINITY;
one - math.NEGATIVE_INFINITY == math.POSITIVE_INFINITY;
-math.POSITIVE_INFINITY == math.NEGATIVE_INFINITY;
-math.NEGATIVE_INFINITY == math.POSITIVE_INFINITY;
math.POSITIVE_INFINITY - one == math.POSITIVE_INFINITY;
math.NEGATIVE_INFINITY - one == math.NEGATIVE_INFINITY;
math.isNaN(math.POSITIVE_INFINITY - math.POSITIVE_INFINITY ) == true;
math.isNaN(math.NEGATIVE_INFINITY - math.NEGATIVE_INFINITY) == true;
math.POSITIVE_INFINITY - math.NEGATIVE_INFINITY == math.POSITIVE_INFINITY;
math.NEGATIVE_INFINITY - math.POSITIVE_INFINITY == math.NEGATIVE_INFINITY;
math.isNaN(math.POSITIVE_INFINITY - math.NaN) == true;
math.isNaN(math.NEGATIVE_INFINITY - math.NaN) == true;
math.isNaN(math.NaN - math.POSITIVE_INFINITY) == true;
math.isNaN(math.NaN - math.NEGATIVE_INFINITY) == true;
// *
math.POSITIVE_INFINITY * one == math.POSITIVE_INFINITY;
math.NEGATIVE_INFINITY * one == math.NEGATIVE_INFINITY;
math.isNaN(math.POSITIVE_INFINITY * zero) == true;
math.isNaN(math.NEGATIVE_INFINITY * zero) == true;
math.POSITIVE_INFINITY * math.POSITIVE_INFINITY == math.POSITIVE_INFINITY;
math.NEGATIVE_INFINITY * math.NEGATIVE_INFINITY  == math.POSITIVE_INFINITY;
math.POSITIVE_INFINITY * math.NEGATIVE_INFINITY == math.NEGATIVE_INFINITY;
math.isNaN(math.POSITIVE_INFINITY * math.NaN) == true;
math.isNaN(math.NEGATIVE_INFINITY * math.NaN) == true;
// /
math.POSITIVE_INFINITY / one == math.POSITIVE_INFINITY;
math.NEGATIVE_INFINITY / one == math.NEGATIVE_INFINITY;
//math.POSITIVE_INFINITY / zero == math.POSITIVE_INFINITY;
//math.NEGATIVE_INFINITY / zero == math.NEGATIVE_INFINITY;
math.isNaN(math.POSITIVE_INFINITY / math.POSITIVE_INFINITY);
math.isNaN(math.POSITIVE_INFINITY / math.NEGATIVE_INFINITY);
math.isNaN(math.NEGATIVE_INFINITY / math.POSITIVE_INFINITY);
math.isNaN(math.NEGATIVE_INFINITY / math.NEGATIVE_INFINITY);
math.isNaN(math.NaN / math.POSITIVE_INFINITY);
math.isNaN(math.POSITIVE_INFINITY / math.NaN);
math.isNaN(math.NaN / math.POSITIVE_INFINITY);
math.isNaN(math.NEGATIVE_INFINITY / math.NaN);

// abs
math.abs(-1.223) == 1.223;
math.abs(1.223) == 1.223;
math.abs(0) == 0;
math.isNaN(math.abs(math.NaN)) == true;
math.abs(math.NEGATIVE_INFINITY) == math.POSITIVE_INFINITY;
math.abs(math.POSITIVE_INFINITY) == math.POSITIVE_INFINITY;

// min
math.min(0.0, 1.0) == 0.0;
math.min(0.0, -1.0) == -1.0;
math.min(0.0, 0.0) == 0.0;
math.min(1.0, 1.0) == 1.0;
math.min(math.NEGATIVE_INFINITY, math.NEGATIVE_INFINITY) == math.NEGATIVE_INFINITY;
math.min(math.NEGATIVE_INFINITY, math.POSITIVE_INFINITY) == math.NEGATIVE_INFINITY;
math.min(math.POSITIVE_INFINITY, math.POSITIVE_INFINITY) == math.POSITIVE_INFINITY;
math.min(math.POSITIVE_INFINITY, zero) == zero;
math.min(math.NEGATIVE_INFINITY, zero) == math.NEGATIVE_INFINITY;
math.isNaN(math.min(math.NEGATIVE_INFINITY, math.NaN)) == true;
math.isNaN(math.min(math.POSITIVE_INFINITY, math.NaN)) == true;
math.isNaN(math.min(math.NaN, math.NaN)) == true;
math.isNaN(math.min(one, math.NaN)) == true;
math.isNaN(math.min(zero, math.NaN)) == true;
math.isNaN(math.min(math.NaN, math.NEGATIVE_INFINITY)) == true;
math.isNaN(math.min(math.NaN,math.POSITIVE_INFINITY)) == true;
math.isNaN(math.min(math.NaN, one)) == true;
math.isNaN(math.min(math.NaN, zero)) == true;

// max
math.max(0.0, 1.0) == 1.0;
math.max(0.0, -1.0) == 0.0;
math.max(0.0, 0.0) == 0.0;
math.max(1.0, 1.0) == 1.0;
math.max(math.NEGATIVE_INFINITY, math.NEGATIVE_INFINITY) == math.NEGATIVE_INFINITY;
math.max(math.NEGATIVE_INFINITY, math.POSITIVE_INFINITY) == math.POSITIVE_INFINITY;
math.max(math.POSITIVE_INFINITY, math.POSITIVE_INFINITY) == math.POSITIVE_INFINITY;
math.max(math.POSITIVE_INFINITY, zero) == math.POSITIVE_INFINITY;
math.max(math.NEGATIVE_INFINITY, zero) == 0;
math.isNaN(math.max(math.NEGATIVE_INFINITY, math.NaN)) == true;
math.isNaN(math.max(math.POSITIVE_INFINITY, math.NaN)) == true;
math.isNaN(math.max(math.NaN, math.NaN)) == true;
math.isNaN(math.max(one, math.NaN)) == true;
math.isNaN(math.max(zero, math.NaN)) == true;
math.isNaN(math.max(math.NaN, math.NEGATIVE_INFINITY)) == true;
math.isNaN(math.max(math.NaN,math.POSITIVE_INFINITY)) == true;
math.isNaN(math.max(math.NaN, one)) == true;
math.isNaN(math.max(math.NaN, zero)) == true;

// sin
math.sin(0.0) == 0.0;
math.sin(math.PI / 2) == 1.0;
math.sin(math.PI) == 0.0;
math.sin(math.PI * 3 / 2) == -1.0;
math.isNaN(math.sin(math.POSITIVE_INFINITY)) == true;
math.isNaN(math.sin(math.NEGATIVE_INFINITY)) == true;
math.isNaN(math.sin(math.NaN)) == true;

// cos
math.cos(0.0) == 1.0;
math.cos(math.PI / 2) == 0.0;
math.cos(math.PI) == -1.0;
math.cos(math.PI * 3 / 2) == 0.0;
math.isNaN(math.cos(math.POSITIVE_INFINITY)) == true;
math.isNaN(math.cos(math.NEGATIVE_INFINITY)) == true;
math.isNaN(math.cos(math.NaN)) == true;

// exp
math.exp(0.0) == 1.0;
math.exp(1.0) == 2.7182818284590452353602874713527;
math.exp(math.POSITIVE_INFINITY) == math.POSITIVE_INFINITY;
math.exp(math.NEGATIVE_INFINITY) == 0.0;
math.isNaN(math.exp(math.NaN)) == true;

// log
math.log(0.0) == math.NEGATIVE_INFINITY;
math.log(2.7182818284590452353602874713527) == 1.0;
math.isNaN(math.log( -1.0)) == true;
math.isNaN(math.log(math.NaN)) == true;
math.isNaN(math.log(math.NEGATIVE_INFINITY)) == true;
math.log(math.POSITIVE_INFINITY) == math.POSITIVE_INFINITY;

// exp + log
var floats = [1.33, 12.0, -112.999992, 0.0, math.NEGATIVE_INFINITY, math.POSITIVE_INFINITY];
for (f in floats) {
	#if !php
	feq(math.log(math.exp(f)), f);
	#end
}

// sqrt
math.sqrt(4.0) == 2;
math.sqrt(0.0) == 0.0;
math.sqrt(math.POSITIVE_INFINITY) == math.POSITIVE_INFINITY;
math.isNaN(math.sqrt(math.NEGATIVE_INFINITY)) == true;
math.isNaN(math.sqrt(math.NaN)) == true;
math.isNaN(math.sqrt( -1.0)) == true;

// round
math.round(0.0) == 0;
math.round(0.1) == 0;
math.round(0.4999) == 0;
math.round(0.5) == 1;
math.round(1.0) == 1;
math.round(1.499) == 1;
math.round(-0.1) == 0;
math.round(-0.4999) == 0;
math.round(-0.5) == 0;
math.round(-0.50001) == -1;
math.round(-1.0) == -1;
math.round(-1.499) == -1;
math.round(-1.5) == -1;
math.round( -1.50001) == -2;
math.fround(math.POSITIVE_INFINITY) == math.POSITIVE_INFINITY;
math.fround(math.NEGATIVE_INFINITY) == math.NEGATIVE_INFINITY;
math.isNaN(math.fround(math.NaN)) == true;

// floor
math.floor(0.0) == 0;
math.floor(0.9999) == 0;
math.floor(1.0) == 1;
math.floor( -0.0001) == -1;
math.floor( -1.0) == -1;
math.floor( -1.0001) == -2;
math.ffloor(math.POSITIVE_INFINITY) == math.POSITIVE_INFINITY;
math.ffloor(math.NEGATIVE_INFINITY) == math.NEGATIVE_INFINITY;
math.isNaN(math.ffloor(math.NaN)) == true;

// ceil
math.ceil(0.0) == 0;
math.ceil(-0.9999) == 0;
math.ceil(-1.0) == -1;
math.ceil( 0.0001) == 1;
math.ceil( 1.0) == 1;
math.ceil( 1.0001) == 2;
math.fceil(math.POSITIVE_INFINITY) == math.POSITIVE_INFINITY;
math.fceil(math.NEGATIVE_INFINITY) == math.NEGATIVE_INFINITY;
math.isNaN(math.fceil(math.NaN)) == true;

// random
// not much to test here...

// isFinite
math.isFinite(math.POSITIVE_INFINITY) == false;
math.isFinite(math.NEGATIVE_INFINITY) == false;
math.isFinite(math.NaN) == false;
math.isFinite(0.0) == true;

// isNaN
math.isNaN(math.POSITIVE_INFINITY) == false;
math.isNaN(math.NEGATIVE_INFINITY) == false;
math.isNaN(math.NaN) == true;
math.isNaN(0.0) == false;



