var u:haxe.Unit = null;
Reflect.isObject(u) == false;
Reflect.isEnumValue(u) == false;
Reflect.isFunction(u) == false;
Reflect.compare(u, null) == 0;
Reflect.compare(null, u) == 0;
Reflect.compare(u, u) == 0;
Type.getClass(u) == null;
Type.getEnum(u) == null;
