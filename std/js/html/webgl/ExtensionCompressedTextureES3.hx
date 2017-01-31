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
	The `WEBGL_compressed_texture_es3` extension is part of the WebGL API and exposes 10 ETC/EAC compressed texture formats.

	Documentation [WEBGL_compressed_texture_es3](https://developer.mozilla.org/en-US/docs/Web/API/WEBGL_compressed_texture_es3) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/WEBGL_compressed_texture_es3$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/WEBGL_compressed_texture_es3>
**/
@:native("WEBGL_compressed_texture_es3")
extern class ExtensionCompressedTextureES3
{
	static inline var COMPRESSED_R11_EAC : Int = 37488;
	static inline var COMPRESSED_SIGNED_R11_EAC : Int = 37489;
	static inline var COMPRESSED_RG11_EAC : Int = 37490;
	static inline var COMPRESSED_SIGNED_RG11_EAC : Int = 37491;
	static inline var COMPRESSED_RGB8_ETC2 : Int = 37492;
	static inline var COMPRESSED_SRGB8_ETC2 : Int = 37493;
	static inline var COMPRESSED_RGB8_PUNCHTHROUGH_ALPHA1_ETC2 : Int = 37494;
	static inline var COMPRESSED_SRGB8_PUNCHTHROUGH_ALPHA1_ETC2 : Int = 37495;
	static inline var COMPRESSED_RGBA8_ETC2_EAC : Int = 37496;
	static inline var COMPRESSED_SRGB8_ALPHA8_ETC2_EAC : Int = 37497;
	
}