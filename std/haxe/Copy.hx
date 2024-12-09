package haxe;

class Copy {
	var cacheMap:Array<Dynamic>;
	var cache:Array<Dynamic>;

	public function new() {
		cacheMap = [];
		cache = [];
	}

	public function copyValue(v:Dynamic):Dynamic {
		switch (Type.typeof(v)) {
			case TNull, TInt, TFloat, TBool:
				return v;
			case TClass(c):
				if (#if neko untyped c.__is_String #else c == String #end)
					return v;
				var index = getRefIndex(v);
				if (index >= 0)
					return cacheMap[index];
				switch (#if (neko || python) Type.getClassName(c) #else c #end) {
					case #if (neko || python) "Array" #else cast Array #end:
						var nv:Array<Dynamic> = [];
						cacheMap.push(nv);
						#if (flash || python || hl)
						var v:Array<Dynamic> = v;
						#end
						var l = v.length;
						for (i in 0...l) {
							var e:Dynamic = v[i];
							if (e == null)
								nv.push(null);
							else
								nv.push(copyValue(e));
						}
						return nv;
					case #if (neko || python) "haxe.ds.List" #else cast List #end:
						var nv = new List<Dynamic>();
						cacheMap.push(nv);
						var v:List<Dynamic> = v;
						for (e in v)
							nv.add(copyValue(e));
						return nv;
					case #if (neko || python) "Date" #else cast Date #end:
						return v;
					case #if (neko || python) "haxe.ds.StringMap" #else cast haxe.ds.StringMap #end:
						var nv = new haxe.ds.StringMap<Dynamic>();
						cacheMap.push(nv);
						var v:haxe.ds.StringMap<Dynamic> = v;
						for (k => e in v)
							nv.set(k, copyValue(e));
						return nv;
					case #if (neko || python) "haxe.ds.IntMap" #else cast haxe.ds.IntMap #end:
						var nv = new haxe.ds.IntMap<Dynamic>();
						cacheMap.push(nv);
						var v:haxe.ds.IntMap<Dynamic> = v;
						for (k => e in v)
							nv.set(k, copyValue(e));
						return nv;
					case #if (neko || python) "haxe.ds.ObjectMap" #else cast haxe.ds.ObjectMap #end:
						var nv = new haxe.ds.ObjectMap<Dynamic, Dynamic>();
						cacheMap.push(nv);
						var v:haxe.ds.ObjectMap<Dynamic, Dynamic> = v;
						for (k => e in v) {
							#if (js || neko)
							var id = Reflect.field(k, "__id__");
							Reflect.deleteField(k, "__id__");
							nv.set(k, copyValue(e));
							Reflect.setField(k, "__id__", id);
							#else
							nv.set(k, copyValue(e));
							#end
						}
						return nv;
					case #if (neko || python) "haxe.io.Bytes" #else cast haxe.io.Bytes #end:
						var v:haxe.io.Bytes = v;
						var nv = v.sub(0, v.length);
						cacheMap.push(nv);
						return nv;
					default:
						var nv = Type.createEmptyInstance(c);
						cacheMap.push(nv);
						copyFields(v, nv);
						return nv;
				}
			case TObject:
				if (v is Class || v is Enum)
					return v;
				var index = getRefIndex(v);
				if (index >= 0)
					return cacheMap[index];
				var nv = {};
				cacheMap.push(nv);
				copyFields(v, nv);
				return nv;
			case TEnum(e):
				var index = getRefIndex(v);
				if (index >= 0)
					return cacheMap[index];
				var v:EnumValue = v;
				var args = v.getParameters();
				if (args.length == 0) {
					cacheMap.push(v);
					return v;
				}
				var nv:Dynamic = Type.createEnumIndex(e, v.getIndex(), args);
				var needCopy = false;
				cacheMap.push(nv);
				for (i => e in args) {
					var e = copyValue(e);
					#if neko
					nv.args[i] = e;
					#elseif php
					nv.params[i] = e;
					#elseif (js && !js_enums_as_arrays)
					nv.__params__[i] = e;
					#elseif js
					nv[i + 2] = e;
					#else
					needCopy = true;
					args[i] = e;
					#end
				}
				// only for platforms that don't know how to modify enum after create
				// as a result, this might break some circular depencies
				if (needCopy)
					nv = Type.createEnumIndex(e, v.getIndex(), args);
				return nv;
			default:
				return v; // assume not mutable
		}
	}

	function getRefIndex(v:Dynamic) {
		#if js
		var vt = js.Syntax.typeof(v);
		#end
		for (i in 0...cache.length) {
			#if js
			var ci = cache[i];
			if (js.Syntax.typeof(ci) == vt && ci == v)
			#else
			if (cache[i] == v)
			#end
			return i;
		}
		cache.push(v);
		return -1;
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
