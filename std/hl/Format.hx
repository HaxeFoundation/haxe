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

package hl;

enum abstract PixelFormat(Int) {
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
	@:hlNative("fmt", "jpg_decode")
	public static function decodeJPG(src:hl.Bytes, srcLen:Int, dst:hl.Bytes, width:Int, height:Int, stride:Int, format:PixelFormat, flags:Int):Bool {
		return false;
	}

	/**
		Decode PNG data into the target buffer.
	**/
	@:hlNative("fmt", "png_decode")
	public static function decodePNG(src:hl.Bytes, srcLen:Int, dst:hl.Bytes, width:Int, height:Int, stride:Int, format:PixelFormat, flags:Int):Bool {
		return false;
	}

	/**
		Decode any image data into ARGB pixels
	**/
	#if (hl_ver >= version("1.10.0"))
	@:hlNative("fmt", "dxt_decode")
	public static function decodeDXT(src:hl.Bytes, dst:hl.Bytes, width:Int, height:Int, dxtFormat:Int):Bool {
		return false;
	}
	#end

	/**
		Upscale/downscale an image.
		Currently supported flag bits: 1 = bilinear filtering
	**/
	@:hlNative("fmt", "img_scale")
	public static function scaleImage(out:hl.Bytes, outPos:Int, outStride:Int, outWidth:Int, outHeight:Int, _in:hl.Bytes, inPos:Int, inStride:Int,
		inWidth:Int, inHeight:Int, flags:Int) {}

	/**
		Performs a cryptographic digest of some bytes.
		0 = Md5 , 1 = Sha1 , 2 = Crc32, 3 = Adler32
		Set 256 flag to tell the src are String bytes.
	**/
	@:hlNative("fmt", "digest")
	public static function digest(out:hl.Bytes, src:hl.Bytes, srcLen:Int, algorithm:Int) {}
}

class Mikktspace {
	public var buffer:hl.BytesAccess<Single>;
	public var stride:Int;
	public var xPos:Int;
	public var normalPos:Int;
	public var uvPos:Int;
	public var tangents:hl.BytesAccess<Single>;
	public var tangentStride:Int;
	public var tangentPos:Int;
	public var indexes:hl.BytesAccess<Int>;
	public var indices:Int;

	public function new() {}

	public function compute(threshold = 180.) {
		if (!_compute(this, threshold))
			throw "assert";
	}

	@:hlNative("fmt", "compute_mikkt_tangents") static function _compute(m:Dynamic, threshold:Float):Bool {
		return false;
	}
}
