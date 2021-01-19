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

package jvm;

import java.NativeArray;

@:keep
@:native('haxe.jvm.Enum')
class Enum<T:EnumValue> extends java.lang.Enum<T> {
	@:nativeGen public function new(index:Int, name:String) {
		super(name, index);
	}

	@:overload public function equals<T:EnumValue>(other:Enum<T>) {
		return super.equals(other);
	}

	@:jvm.synthetic public function _hx_getParameters() {
		return new java.NativeArray(0);
	}

	@:overload
	override public function toString() {
		var baseName = Type.getEnumConstructs(Type.getEnum(cast this))[ordinal()];
		var parameters = Type.enumParameters(cast this);
		if (parameters.length == 0) {
			return baseName;
		}
		return '$baseName(${@:privateAccess parameters.join(",")})';
	}
}
