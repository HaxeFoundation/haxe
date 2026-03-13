/*
Copyright (c) 2017-2019 Guillaume Desquesnes, Valentin Lemière

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
*/

package json2object.reader;

import haxe.Constraints.Function;
import json2object.Error;
import json2object.Error.ErrorType;
import json2object.Position;
import json2object.PositionUtils;

using StringTools;

#if cs @:nativeGen #end
class BaseParser<T> {

	public var value : T;

	public var errors:Array<Error>;

	private var errorType:Error.ErrorType;

	private var putils:PositionUtils;

	private function new(errors:Array<Error>, putils:PositionUtils, errorType:ErrorType) {
		this.errors = errors;
		this.putils = putils;
		this.errorType = errorType;
	}

	public function fromJson(jsonString:String, filename:String="") : T {
		putils = new PositionUtils(jsonString);
		errors = [];
		try {
			var json = hxjsonast.Parser.parse(jsonString, filename);
			loadJson(json);
		}
		catch (e:hxjsonast.Error) {
			errors.push(ParserError(e.message, putils.convertPosition(e.pos)));
		}
		return value;
	}

	public function loadJson(json:hxjsonast.Json, variable:String="") : T {
		var pos = putils.convertPosition(json.pos);
		switch (json.value) {
			case JNull : loadJsonNull(pos, variable);
			case JString(s) : loadJsonString(s, pos, variable);
			case JNumber(n) : loadJsonNumber(n, pos, variable);
			case JBool(b) : loadJsonBool(b, pos, variable);
			case JArray(a) : loadJsonArray(a, pos, variable);
			case JObject(o) : loadJsonObject(o, pos, variable);
		}
		return value;
	}

	private function loadJsonNull(pos:Position, variable:String) {
		onIncorrectType(pos, variable);
	}

	private function loadJsonString(s:String, pos:Position, variable:String) {
		onIncorrectType(pos, variable);
	}

	private function loadString(s:String, pos:Position, variable:String, validValues:Array<String>, defaultValue:String):String {
		if (validValues.indexOf(s) != -1) {
			return s;
		}
		onIncorrectType(pos, variable);
		return defaultValue;
	}

	private function loadJsonNumber(f:String, pos:Position, variable:String) {
		onIncorrectType(pos, variable);
	}

	private function loadJsonUInt(f:String, pos:Position, variable:String, value:UInt):UInt {
		var uint:UInt = 0;
		f = f.trim();
		var neg = f.charAt(0) == '-';
		if (neg) {
			f = f.substr(1);
		}
		var hex = f.startsWith('0x');
		if (hex) {
			f = f.substr(2);
		}

		var base = hex ? 16 : 10;
		var pow = 1;
		var i = f.length - 1;
		while (i >= 0) {
			var cur = hex ? Std.parseInt('0x${f.charAt(i)}') : Std.parseInt(f.charAt(i));
			if (cur == null) {
				onIncorrectType(pos, variable);
				return value;
			}
			uint += pow * cur;
			pow *= base;
			i--;
		}
		return uint;
	}

	private function loadJsonInt(f:String, pos:Position, variable:String, value:Int):Int {
		if (Std.parseInt(f) != null && Std.parseInt(f) == Std.parseFloat(f)) {
			return Std.parseInt(f);
		}
		onIncorrectType(pos, variable);
		return value;
	}

	private function loadJsonFloat(f:String, pos:Position, variable:String, value:Float):Float {
		if (Std.parseInt(f) != null) {
			return Std.parseFloat(f);
		}
		onIncorrectType(pos, variable);
		return value;
	}

	private function loadJsonBool(b:Bool, pos:Position, variable:String) {
		onIncorrectType(pos, variable);
	}

	private function loadJsonArray(a:Array<hxjsonast.Json>, pos:Position, variable:String) {
		onIncorrectType(pos, variable);
	}

	private function loadJsonArrayValue(a:Array<hxjsonast.Json>, loadJsonFn:Function, variable:String) {
		return [
			for (j in a)
			{
				try {
					loadJsonFn(j, variable);
				} catch (e:InternalError) {
					if (e != ParsingThrow) {
						throw e;
					}

					continue;
				}
			}
		];
	}

	private function loadJsonObject(o:Array<hxjsonast.Json.JObjectField>, pos:Position, variable:String) {
		onIncorrectType(pos, variable);
	}

	private function loadObjectField(loadJsonFn:Function, field:hxjsonast.Json.JObjectField, name:String, assigned:Map<String, Bool>, defaultValue:Any, pos:Position):Any {
		try {
			var ret = cast loadJsonFn(field.value, field.name);
			mapSet(assigned, name, true);
			return ret;
		} catch (e:InternalError) {
			if (e != ParsingThrow) {
				throw e;
			}
		}
		#if cs
		// CS sometimes wrap the Haxe errors, unwrap them.
		// Could be https://github.com/HaxeFoundation/haxe/issues/6817
		catch (e:cs.system.reflection.TargetInvocationException) {
			#if (haxe_ver >= 4.1)
			var e = cast(e.InnerException, haxe.ValueException).value;
			var es = '$e';
			#elseif haxe4
			var e = untyped __cs__("((global::haxe.lang.HaxeException)(e.InnerException)).obj");
			var es = e.toString();
			#else
			var e = untyped __cs__("((global::haxe.lang.HaxeException)(global::haxe.lang.Exceptions.exception.InnerException)).obj");
			var es = e.toString();
			#end

			if (es != "ParsingThrow") {
				errors.push(CustomFunctionException(e, pos));
			}
		}
		#end
		catch (e:Any) {
			errors.push(CustomFunctionException(e, pos));
		}
		return defaultValue;
	}

	private function loadObjectFieldReflect(loadJsonFn:Function, field:hxjsonast.Json.JObjectField, name:String, assigned:Map<String, Bool>, pos:Position) {
		try {
			Reflect.setField(value, name, cast loadJsonFn(field.value, field.name));
			mapSet(assigned, name, true);
		} catch (e:InternalError) {
			if (e != ParsingThrow) {
				throw e;
			}
		}
		catch (e:Any) {
			errors.push(CustomFunctionException(e, pos));
		}
	}

	private function objectSetupAssign(assigned:Map<String, Bool>, keys:Array<String>, values:Array<Bool>) {
		for (i in 0...keys.length) {
			mapSet(assigned, keys[i], values[i]);
		}
	}

	private function objectErrors(assigned:Map<String, Bool>, pos:Position) {
		var lastPos:Null<json2object.Position> = putils.convertPosition({file:pos.file, min:pos.max-1, max:pos.max-1});
		for (s in assigned.keys()) {
			if (!assigned[s]) {
				errors.push(UninitializedVariable(s, lastPos));
			}
		}
	}

	private function onIncorrectType(pos:Position, variable:String) {
		parsingThrow();
	}

	private function parsingThrow() {
		if (errorType != NONE) {
			throw ParsingThrow;
		}
	}

	private function objectThrow(pos:Position, variable:String) {
		if (errorType == THROW) {
			throw ParsingThrow;
		}

		if (errorType == OBJECTTHROW) {
			errors.push(UninitializedVariable(variable, pos));
		}
	}

	private #if !js inline #end function mapSet(map:Map<String, Bool>, key:String, value:Bool) {
		map.set(key, value);
	}
}
