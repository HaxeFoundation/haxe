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
	The `EXT_texture_filter_anisotropic` extension is part of the WebGL API and exposes two constants for anisotropic filtering (AF).

	Documentation [EXT_texture_filter_anisotropic](https://developer.mozilla.org/en-US/docs/Web/API/EXT_texture_filter_anisotropic) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/EXT_texture_filter_anisotropic$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/EXT_texture_filter_anisotropic>
**/
@:native("EXT_texture_filter_anisotropic")
extern class EXTTextureFilterAnisotropic
{
	
	/**
		This is the `pname` argument to the `WebGLRenderingContext.getTexParameter` and `WebGLRenderingContext.texParameterf` / `WebGLRenderingContext.texParameteri` calls and sets the desired maximum anisotropy for a texture.
	**/
	static inline var TEXTURE_MAX_ANISOTROPY_EXT : Int = 34046;
	
	/**
		This is the `pname` argument to the `WebGLRenderingContext.getParameter` call, and it returns the maximum available anisotropy.
	**/
	static inline var MAX_TEXTURE_MAX_ANISOTROPY_EXT : Int = 34047;
	
}