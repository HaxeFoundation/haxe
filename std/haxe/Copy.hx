package haxe;

import haxe.ds.StringMap;
import haxe.ds.IntMap;
import haxe.ds.ObjectMap;
import haxe.io.Bytes;

class Copy {
	// TODO: check __id__ stuff on JS/neko
	var cacheMap:ObjectMap<{}, {}>;
	var cacheMapLength:Int;

	function new() {
		cacheMap = new ObjectMap();
		cacheMapLength = 0;
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
						cacheMap.set(v, a);
						var v:Array<Dynamic> = cast v;
						for (x in v) {
							if (x == null) {
								a.push(null);
							} else {
								a.push(copyValue(x));
							}
						}
						cast a;
					case haxe.ds.StringMap:
						var map = new StringMap();
						cacheMap.set(v, map);
						var v:StringMap<Dynamic> = cast v;
						for (k => v in v) {
							map.set(k, copyValue(v));
						}
						cast map;
					case haxe.ds.IntMap:
						var map = new IntMap();
						cacheMap.set(v, map);
						var v:IntMap<Dynamic> = cast v;
						for (k => v in v) {
							map.set(k, copyValue(v));
						}
						cast map;
					case haxe.ds.ObjectMap:
						var map = new ObjectMap();
						cacheMap.set(v, map);
						var v:ObjectMap<{}, Dynamic> = cast v;
						for (k => v in v) {
							// TODO: check the __id__ situation
							map.set(copyValue(k), copyValue(v));
						}
						cast map;
					case haxe.io.Bytes:
						var v:Bytes = cast v;
						var nv = v.sub(0, v.length);
						cacheMap.set(v, nv);
						cast nv;
					case _:
						vCopy = Type.createEmptyInstance(c);
						cacheMap.set(v, vCopy);
						copyFields(v, vCopy);
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
				cacheMap.set(v, o);
				copyFields(v, o);
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
					cacheMap.set(v, v);
					return v;
				}
				var newArgs = [];
				for (arg in args) {
					// TODO: check wtf was happening here in the original implementation
					newArgs.push(copyValue(arg));
				}
				var nv:O = cast Type.createEnumIndex(en, vEnumValue.getIndex(), newArgs);
				cacheMap.set(v, nv);
				nv;
			case TUnknown | TFunction:
				v;
		}
	}

	function getRef<T:{}>(v:T):T {
		var vCopy = cacheMap.get(v);
		if (vCopy != null) {
			return cast vCopy;
		}
		return null;
	}

	function copyFields(v:Dynamic, nv:Dynamic) {
		for (f in Reflect.fields(v)) {
			var e = copyValue(Reflect.field(v, f));
			Reflect.setField(nv, f, e);
		}
	}

	public static function copy<T>(v:T):T {
		var s = new Copy();
		return s.copyValue(v);
	}
}
