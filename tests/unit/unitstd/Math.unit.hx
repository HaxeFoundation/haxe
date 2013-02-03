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
var floats = [1.33, 39232.911, 12.0, -112.999992, 99999.99999, 0.0, Math.NEGATIVE_INFINITY, Math.POSITIVE_INFINITY];
for (f in floats) {
	Math.log(Math.exp(f)) == f;
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

// floor
Math.floor(0.0) == 0;
Math.floor(0.9999) == 0;
Math.floor(1.0) == 1;
Math.floor( -0.0001) == -1;
Math.floor( -1.0) == -1;
Math.floor( -1.0001) == -2;
Math.ffloor(Math.POSITIVE_INFINITY) == Math.POSITIVE_INFINITY;
Math.ffloor(Math.NEGATIVE_INFINITY) == Math.NEGATIVE_INFINITY;
Math.isNaN(Math.floor(Math.NaN)) == true;

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