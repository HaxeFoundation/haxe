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

package cs;

@:keep
@:native('haxe.root.HaxeObject')
@:nativeGen
class HaxeObject {
	public function new() {}

	public function _hx_getField(name:String):Dynamic {
		return null;
	}

	public function _hx_setField(name:String, value:Dynamic):Void {}

	public function _hx_deleteField(name:String):Bool {
		return false;
	}

	public function _hx_getFields():Array<String> {
		return [];
	}

	// Method invocation dispatchers for MethodClosure
	// Override in subclasses to dispatch by method index

	public function _hx_invokeMethod0(index:Int):Value {
		throw "Method not found: index " + index;
	}

	public function _hx_invokeMethod1(index:Int, a1:Value):Value {
		throw "Method not found: index " + index;
	}

	public function _hx_invokeMethod2(index:Int, a1:Value, a2:Value):Value {
		throw "Method not found: index " + index;
	}

	public function _hx_invokeMethod3(index:Int, a1:Value, a2:Value, a3:Value):Value {
		throw "Method not found: index " + index;
	}

	public function _hx_invokeMethod4(index:Int, a1:Value, a2:Value, a3:Value, a4:Value):Value {
		throw "Method not found: index " + index;
	}

	public function _hx_invokeMethod5(index:Int, a1:Value, a2:Value, a3:Value, a4:Value, a5:Value):Value {
		throw "Method not found: index " + index;
	}

	public function _hx_invokeMethod6(index:Int, a1:Value, a2:Value, a3:Value, a4:Value, a5:Value, a6:Value):Value {
		throw "Method not found: index " + index;
	}

	public function _hx_invokeMethod7(index:Int, a1:Value, a2:Value, a3:Value, a4:Value, a5:Value, a6:Value, a7:Value):Value {
		throw "Method not found: index " + index;
	}

	public function _hx_invokeMethod8(index:Int, a1:Value, a2:Value, a3:Value, a4:Value, a5:Value, a6:Value, a7:Value, a8:Value):Value {
		throw "Method not found: index " + index;
	}

	public function _hx_invokeMethod9(index:Int, a1:Value, a2:Value, a3:Value, a4:Value, a5:Value, a6:Value, a7:Value, a8:Value, a9:Value):Value {
		throw "Method not found: index " + index;
	}

	public function _hx_invokeMethodDynamic(index:Int, args:Array<Dynamic>):Dynamic {
		throw "Method not found: index " + index;
	}
}
