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
package hl;

@:enum abstract PixelFormat(Int) {
  var RGB = 0;
  var BGR = 1;
  var RGBX = 2;
  var BGRX = 3;
  var XBGR = 4;
  var XRGB = 5;
  var GRAY = 6;
  var RGBA = 7;
  var BGRA = 8;
  var ABGR = 9;
  var ARGB = 10;
  var CMYK = 11;
}

/**
	These are the bindings for the HL `fmt.hdll` library, which contains various low level formats handling.
**/
class Format {

	/**
		Decode JPG data into the target buffer.
	**/
	@:hlNative("fmt","jpg_decode")
	public static function decodeJPG( src : hl.Bytes, srcLen : Int, dst : hl.Bytes, width : Int, height : Int, stride : Int, format : PixelFormat, flags : Int ) : Bool {
		return false;
	}

	/**
		Decode PNG data into the target buffer.
	**/
	@:hlNative("fmt","png_decode")
	public static function decodePNG( src : hl.Bytes, srcLen : Int, dst : hl.Bytes, width : Int, height : Int, stride : Int, format : PixelFormat, flags : Int ) : Bool {
		return false;
	}

	/**
		Upscale/downscale an image.
		Currently supported flag bits: 1 = bilinear filtering
	**/
	@:hlNative("fmt","img_scale")
	public static function scaleImage( out : hl.Bytes, outPos : Int,  outStride : Int, outWidth : Int, outHeight : Int, _in : hl.Bytes, inPos : Int,  inStride : Int, inWidth : Int, inHeight : Int, flags : Int ) {
	}

}