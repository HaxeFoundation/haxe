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
import lua.Lua;
import lua.TableTools;
import lua.Boot;
@:coreApi class Reflect {

	public inline static function hasField( o : Dynamic, field : String ) : Bool {
		return untyped o.__fields__ != null ? o.__fields__[field] != null :  o[field] != null;
	}

	public static function field( o : Dynamic, field : String ) : Dynamic untyped {
		return try o[field] catch( e : Dynamic ) null;
	}

	public inline static function setField( o : Dynamic, field : String, value : Dynamic ) : Void untyped {
		o[field] = value;
	}

	public static inline function getProperty( o : Dynamic, field : String ) : Dynamic {
		var tmp : Dynamic;
		return if( o == null ) {
				untyped __define_feature__("Reflect.getProperty",null);
			} else if( o.__properties__ != null && Reflect.field(o, "get_" + field) != null){
				callMethod(o, Reflect.field(o,"get_" + field), []);
			} else {
				Reflect.field(o,field);
			}
	}

	public static inline function setProperty( o : Dynamic, field : String, value : Dynamic ) : Void untyped {
		var tmp : String;
		if( o.__properties__ && o.__properties__["set_"+field]) {
			tmp = o.__properties__["set_" + field];
			if (o.__name__ != null){
				callMethod(null,Reflect.field(o, tmp), [value]) ;
			} else {
				callMethod(o,Reflect.field(o, tmp), [value]) ;
			}
		} else {
			o[field] = __define_feature__("Reflect.setProperty",value);
		}
	}

	public static function callMethod( o : Dynamic, func : haxe.Constraints.Function, args : Array<Dynamic> ) : Dynamic  {
		if (args == null || args.length == 0){
			return func(o);
		} else {
			var self_arg = false;
			if (o != null ){
				// if o is not null, it means we need to pass it as the "self"
				// parameter.  However, we should also check to see if it's
				// a valid class instance in the first place.
				// TODO: Find a more flexible way of determining if we need
				// self or not
				self_arg = true;
			}
			var new_args = lua.Table.create();
			for (i in 0...args.length){
				// copy args to 1-indexed table
				new_args[i + 1] = args[i];
			}
			return if (self_arg){
				// call with o as leading self param
				func(o, lua.TableTools.unpack(new_args, 1, TableTools.maxn(new_args)));
			} else {
				// call with no self param
				func(lua.TableTools.unpack(new_args, 1,  TableTools.maxn(new_args)));
			}
		}
	}

	public static function fields( o : Dynamic ) : Array<String> {
		return [for (f in lua.Boot.fieldIterator(o)) f];
	}

	public static function isFunction( f : Dynamic ) : Bool {
		return Lua.type(f) == "function" && !(Boot.isClass(f) || Boot.isEnum(f));
	}

	public static function compare<T>( a : T, b : T ) : Int {
		if (a == b) return 0
		else if (a == null) return -1
		else if (b == null) return 1
		else return (cast a) > (cast b) ? 1 : -1;
	}

	public static function compareMethods( f1 : Dynamic, f2 : Dynamic ) : Bool {
		return f1 == f2;
	}

	public static function isObject( v : Dynamic ) : Bool untyped {
		if( v == null )
			return false;
		var t = __lua__("type(v)");
		return (t == "string" || (t == "table" && v.__enum__ == null)) || (t == "function" && (lua.Boot.isClass(v) || lua.Boot.isEnum(v)) != null);
	}

	public static function isEnumValue( v : Dynamic ) : Bool {
		return v != null && Std.is(v,lua.Table) && v.__enum__ != null;
	}

	public static function deleteField( o : Dynamic, field : String ) : Bool untyped {
		if( !hasField(o,field) ) return false;
		o[field] = null;
		o.__fields__[field]=null;
		return true;
	}

	public static function copy<T>( o : T ) : T {
		var o2 : Dynamic = {};
		for( f in Reflect.fields(o) )
			Reflect.setField(o2,f,Reflect.field(o,f));
		return o2;
	}

	@:overload(function( f : Array<Dynamic> -> Void ) : Dynamic {})
	public static function makeVarArgs( f : Array<Dynamic> -> Dynamic ) : Dynamic {
		/*
			- Convert var arg to table
			- Set indexing from zero
			- Extract real table length
			- Recreate as dynamic array
			- Return function called with this array
		*/
		return untyped __lua__("function(...)
			local a = {...}
			local b = {}
			local l = 0
			for k, v in pairs(a) do
				b[k-1] = v
				l = math.max(k,l)
			end
			return f(_hx_tab_array(b, l))
		end");
	}

}
