package haxe.runtime;

import haxe.ds.StringMap;
import haxe.ds.IntMap;
import haxe.ds.ObjectMap;
import haxe.ds.List;
import haxe.io.Bytes;

// Python struggles with arrays as ObjectMap keys
// Neko and js add __id__ which isn't great
#if (python || js || neko)
private class ObjectCache<K:{}> {
	var from:Array<K>;
	var to:Array<K>;

	public function new() {
		from = [];
		to = [];
	}

	public function get(k:K) {
		for (i => v in from) {
			if (v == k) {
				return to[i];
			}
		}
		return null;
	}

	public function set(k:K, v:K) {
		var index = from.length;
		from[index] = k;
		to[index] = v;
	}
}
#else
private class ObjectCache<K:{}> {
	var cache:ObjectMap<K, K>;

	public function new() {
		cache = new ObjectMap();
	}

	public inline function get(k:K) {
		return cache.get(k);
	}

	public inline function set(k:K, v:K) {
		cache.set(k, v);
	}
}
#end

class Copy {
	var cache:ObjectCache<{}>;
	var workList:Array<() -> Void>;

	function new() {
		cache = new ObjectCache();
		workList = [];
	}

	function defer(f:() -> Void) {
		workList.push(f);
	}

	function copyValue<T, O:{}
		& T>(v:T):T {
		return switch (Type.typeof(v)) {
			case TNull, TInt, TFloat, TBool, TClass(String | Date):
				v;
			case TClass(c):
				var v:O = cast v;
				var vCopy = getRef(v);
				if (vCopy != null) {
					return vCopy;
				}
				switch (c) {
					case Array:
						var a = [];
						cache.set(v, a);
						var v:Array<Dynamic> = cast v;
						defer(() -> {
							for (x in v) {
								if (x == null) {
									a.push(null);
								} else {
									a.push(copyValue(x));
								}
							}
						});
						cast a;
					case haxe.ds.List:
						var l = new List();
						cache.set(v, l);
						var v:List<Dynamic> = cast v;
						defer(() -> {
							for (x in v) {
								l.add(copyValue(x));
							}
						});
						cast l;
					case haxe.ds.StringMap:
						var map = new StringMap();
						cache.set(v, map);
						var v:StringMap<Dynamic> = cast v;
						defer(() -> {
							for (k => v in v) {
								map.set(k, copyValue(v));
							}
						});
						cast map;
					case haxe.ds.IntMap:
						var map = new IntMap();
						cache.set(v, map);
						var v:IntMap<Dynamic> = cast v;
						defer(() -> {
							for (k => v in v) {
								map.set(k, copyValue(v));
							}
						});
						cast map;
					case haxe.ds.ObjectMap:
						var map = new ObjectMap();
						cache.set(v, map);
						var v:ObjectMap<{}, Dynamic> = cast v;
						defer(() -> {
							for (k => v in v) {
								map.set(copyValue(k), copyValue(v));
							}
						});
						cast map;
					case haxe.io.Bytes:
						var v:Bytes = cast v;
						var nv = v.sub(0, v.length);
						cache.set(v, nv);
						cast nv;
					case _:
						vCopy = Type.createEmptyInstance(c);
						cache.set(v, vCopy);
						#if flash
						defer(copyClassFields.bind(v, vCopy, c));
						#else
						defer(copyFields.bind(v, vCopy));
						#end
						vCopy;
				}
			case TObject:
				if (v is Class || v is Enum) {
					return v;
				}
				var v:O = cast v;
				var vCopy = getRef(v);
				if (vCopy != null) {
					return vCopy;
				}
				var o:O = cast {};
				cache.set(v, o);
				defer(copyFields.bind(v, o));
				o;
			case TEnum(en):
				var v:O = cast v;
				var vEnumValue:EnumValue = cast v;
				var vCopy = getRef(v);
				if (vCopy != null) {
					return vCopy;
				}
				var args = vEnumValue.getParameters();
				if (args.length == 0) {
					cache.set(v, v);
					return v;
				}
				var newArgs = [];
				for (arg in args) {
					newArgs.push(copyValue(arg));
				}
				var nv:O = cast Type.createEnumIndex(en, vEnumValue.getIndex(), newArgs);
				cache.set(v, nv);
				nv;
			case TUnknown | TFunction:
				v;
		}
	}

	inline function getRef<T:{}>(v:T):T {
		return cast cache.get(v);
	}

	function copyFields(v:Dynamic, nv:Dynamic) {
		for (f in Reflect.fields(v)) {
			var e = copyValue(Reflect.field(v, f));
			Reflect.setField(nv, f, e);
		}
	}

	function finalize() {
		while (workList.length > 0) {
			workList.pop()();
		}
	}

	#if flash
	function copyClassFields(v:Dynamic, nv:Dynamic, c:Dynamic) {
		var xml:flash.xml.XML = untyped __global__["flash.utils.describeType"](c);
		var vars = xml.factory[0].child("variable");
		for (i in 0...vars.length()) {
			var f = vars[i].attribute("name").toString();
			if (!v.hasOwnProperty(f))
				continue;
			var e = copyValue(Reflect.field(v, f));
			Reflect.setField(nv, f, e);
		}
	}
	#end

	/**
		Creates a deep copy of `v`.

		The following values remain unchanged:

		* null
		* numeric values
		* boolean values
		* strings
		* functions
		* type and enum references (e.g. `haxe.runtime.Copy`, `haxe.ds.Option`)
		* instances of Date
		* enum values without arguments

		Any other value `v` is recursively copied, ensuring
		that `v != copy(v)` holds.
	**/
	public static function copy<T>(v:T):T {
		var copy = new Copy();
		var v = copy.copyValue(v);
		copy.finalize();
		return v;
	}
}
