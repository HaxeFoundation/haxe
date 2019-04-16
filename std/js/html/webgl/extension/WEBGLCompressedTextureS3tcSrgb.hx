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
	The `WEBGL_compressed_texture_s3tc_srgb` extension is part of the WebGL API and exposes four S3TC compressed texture formats for the sRGB colorspace.

	Documentation [WEBGL_compressed_texture_s3tc_srgb](https://developer.mozilla.org/en-US/docs/Web/API/WEBGL_compressed_texture_s3tc_srgb) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/WEBGL_compressed_texture_s3tc_srgb$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/WEBGL_compressed_texture_s3tc_srgb>
**/
@:native("WEBGL_compressed_texture_s3tc_srgb")
extern class WEBGLCompressedTextureS3tcSrgb {
	
	/**
		A DXT1-compressed image in an sRGB image format.
	**/
	static inline var COMPRESSED_SRGB_S3TC_DXT1_EXT : Int = 35916;
	
	/**
		A DXT1-compressed image in an sRGB image format with a simple on/off alpha value.
	**/
	static inline var COMPRESSED_SRGB_ALPHA_S3TC_DXT1_EXT : Int = 35917;
	
	/**
		A DXT3-compressed image in an sRGBA image format.
	**/
	static inline var COMPRESSED_SRGB_ALPHA_S3TC_DXT3_EXT : Int = 35918;
	
	/**
		A DXT5-compressed image in an sRGBA image format.
	**/
	static inline var COMPRESSED_SRGB_ALPHA_S3TC_DXT5_EXT : Int = 35919;
	
}