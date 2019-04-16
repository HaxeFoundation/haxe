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
	The `WEBGL_compressed_texture_s3tc` extension is part of the WebGL API and exposes four S3TC compressed texture formats.

	Documentation [WEBGL_compressed_texture_s3tc](https://developer.mozilla.org/en-US/docs/Web/API/WEBGL_compressed_texture_s3tc) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/WEBGL_compressed_texture_s3tc$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/WEBGL_compressed_texture_s3tc>
**/
@:native("WEBGL_compressed_texture_s3tc")
extern class WEBGLCompressedTextureS3tc {
	
	/**
		A DXT1-compressed image in an RGB image format.
	**/
	static inline var COMPRESSED_RGB_S3TC_DXT1_EXT : Int = 33776;
	
	/**
		A DXT1-compressed image in an RGB image format with a simple on/off alpha value.
	**/
	static inline var COMPRESSED_RGBA_S3TC_DXT1_EXT : Int = 33777;
	
	/**
		A DXT3-compressed image in an RGBA image format. Compared to a 32-bit RGBA texture, it offers 4:1 compression.
	**/
	static inline var COMPRESSED_RGBA_S3TC_DXT3_EXT : Int = 33778;
	
	/**
		A DXT5-compressed image in an RGBA image format. It also provides a 4:1 compression, but differs to the DXT3 compression in how the alpha compression is done.
	**/
	static inline var COMPRESSED_RGBA_S3TC_DXT5_EXT : Int = 33779;
	
}