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

package cpp;

@:native("hx.EnumBase")
extern class EnumBase {
	#if (hxcpp_api_level >= 330)
	public function _hx_getIndex():Int;
	public function _hx_getTag():String;
	public function _hx_getParamCount():Int;
	public function _hx_getParamI(inIndex:Int):Dynamic;
	public function _hx_getParameters():Array<Dynamic>;

	inline public function getIndex():Int
		return _hx_getIndex();
	inline public function getTag():String
		return _hx_getTag();
	inline public function getParamCount():Int
		return _hx_getParamCount();
	inline public function getParamI(inIndex:Int):Dynamic
		return _hx_getParamI(inIndex);
	inline public function getParameters():Array<Dynamic>
		return _hx_getParameters();
	#else
	public function __EnumParams():Array<Dynamic>;
	public function __Tag():String;
	public function __Index():Int;

	inline public function _hx_getIndex():Int
		return untyped __Index();
	inline public function _hx_getTag():String
		return untyped __Tag();
	inline public function _hx_getParamCount():Int
		return untyped __EnumParams() == null ? 0 : __EnumParams().length;
	inline public function _hx_getParamI(inIndex:Int):Dynamic
		return untyped __EnumParams()[inIndex];
	inline public function _hx_getParameters():Array<Dynamic>
		return __EnumParams() == null ? [] : __EnumParams();
	#end
}
