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

// This file is generated from mozilla\WebGLRenderingContext.webidl. Do not edit!

package js.html.webgl;

/**
	The `WEBGL_compressed_texture_atc` extension is part of the WebGL API and exposes 3 ATC compressed texture formats. ATC is a proprietary compression algorithm for compressing textures on handheld devices.

	Documentation [WEBGL_compressed_texture_atc](https://developer.mozilla.org/en-US/docs/Web/API/WEBGL_compressed_texture_atc) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/WEBGL_compressed_texture_atc$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/WEBGL_compressed_texture_atc>
**/
@:native("WEBGL_compressed_texture_atc")
extern class ExtensionCompressedTextureATC
{
	static inline var COMPRESSED_RGB_ATC_WEBGL : Int = 35986;
	static inline var COMPRESSED_RGBA_ATC_EXPLICIT_ALPHA_WEBGL : Int = 35987;
	static inline var COMPRESSED_RGBA_ATC_INTERPOLATED_ALPHA_WEBGL : Int = 34798;
	
}