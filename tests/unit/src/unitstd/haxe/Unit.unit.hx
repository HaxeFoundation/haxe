var u:haxe.Unit = Unit;
Reflect.isObject(u) == false;
Reflect.isEnumValue(u) == true;
Reflect.isFunction(u) == false;
Reflect.compare(u, haxe.Unit.Unit.Unit) == 0;
Reflect.compare(haxe.Unit.Unit.Unit, u) == 0;
Reflect.compare(u, u) == 0;
Type.getClass(u) == null;
Type.getEnum(u) == haxe.Unit;
