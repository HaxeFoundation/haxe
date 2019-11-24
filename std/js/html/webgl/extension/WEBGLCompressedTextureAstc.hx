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

// This file is generated from mozilla\WebGLRenderingContext.webidl. Do not edit!

package js.html.webgl.extension;

/**
	The `WEBGL_compressed_texture_astc` extension is part of the WebGL API and exposes Adaptive Scalable Texture Compression (ASTC) compressed texture formats to WebGL.

	Documentation [WEBGL_compressed_texture_astc](https://developer.mozilla.org/en-US/docs/Web/API/WEBGL_compressed_texture_astc) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/WEBGL_compressed_texture_astc$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/WEBGL_compressed_texture_astc>
**/
@:native("WEBGL_compressed_texture_astc")
extern class WEBGLCompressedTextureAstc {
	static inline var COMPRESSED_RGBA_ASTC_4x4_KHR : Int = 37808;
	static inline var COMPRESSED_RGBA_ASTC_5x4_KHR : Int = 37809;
	static inline var COMPRESSED_RGBA_ASTC_5x5_KHR : Int = 37810;
	static inline var COMPRESSED_RGBA_ASTC_6x5_KHR : Int = 37811;
	static inline var COMPRESSED_RGBA_ASTC_6x6_KHR : Int = 37812;
	static inline var COMPRESSED_RGBA_ASTC_8x5_KHR : Int = 37813;
	static inline var COMPRESSED_RGBA_ASTC_8x6_KHR : Int = 37814;
	static inline var COMPRESSED_RGBA_ASTC_8x8_KHR : Int = 37815;
	static inline var COMPRESSED_RGBA_ASTC_10x5_KHR : Int = 37816;
	static inline var COMPRESSED_RGBA_ASTC_10x6_KHR : Int = 37817;
	static inline var COMPRESSED_RGBA_ASTC_10x8_KHR : Int = 37818;
	static inline var COMPRESSED_RGBA_ASTC_10x10_KHR : Int = 37819;
	static inline var COMPRESSED_RGBA_ASTC_12x10_KHR : Int = 37820;
	static inline var COMPRESSED_RGBA_ASTC_12x12_KHR : Int = 37821;
	static inline var COMPRESSED_SRGB8_ALPHA8_ASTC_4x4_KHR : Int = 37840;
	static inline var COMPRESSED_SRGB8_ALPHA8_ASTC_5x4_KHR : Int = 37841;
	static inline var COMPRESSED_SRGB8_ALPHA8_ASTC_5x5_KHR : Int = 37842;
	static inline var COMPRESSED_SRGB8_ALPHA8_ASTC_6x5_KHR : Int = 37843;
	static inline var COMPRESSED_SRGB8_ALPHA8_ASTC_6x6_KHR : Int = 37844;
	static inline var COMPRESSED_SRGB8_ALPHA8_ASTC_8x5_KHR : Int = 37845;
	static inline var COMPRESSED_SRGB8_ALPHA8_ASTC_8x6_KHR : Int = 37846;
	static inline var COMPRESSED_SRGB8_ALPHA8_ASTC_8x8_KHR : Int = 37847;
	static inline var COMPRESSED_SRGB8_ALPHA8_ASTC_10x5_KHR : Int = 37848;
	static inline var COMPRESSED_SRGB8_ALPHA8_ASTC_10x6_KHR : Int = 37849;
	static inline var COMPRESSED_SRGB8_ALPHA8_ASTC_10x8_KHR : Int = 37850;
	static inline var COMPRESSED_SRGB8_ALPHA8_ASTC_10x10_KHR : Int = 37851;
	static inline var COMPRESSED_SRGB8_ALPHA8_ASTC_12x10_KHR : Int = 37852;
	static inline var COMPRESSED_SRGB8_ALPHA8_ASTC_12x12_KHR : Int = 37853;
	
	function getSupportedProfiles() : Array<String>;
}