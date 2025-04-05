import java.lang.NoSuchMethodException;
import jvm.Jvm;
import jvm.annotation.*;

using jvm.NativeTools.NativeClassTools;
using jvm.NativeTools.NativeEnumTools;
using jvm.NativeTools.ObjectTools;

enum ValueType {
	TNull;
	TInt;
	TFloat;
	TBool;
	TObject;
	TFunction;
	TClass(c:Class<Dynamic>);
	TEnum(e:Enum<Dynamic>);
	TUnknown;
}

@:coreApi
class Type {
	static function isEnumClass<T>(c:java.lang.Class<T>):Bool {
		// TODO: have to be careful if we ever decide to omit EnumReflectionInformation
		// Maybe a separate HaxeEnum annotation would be better here
		return c.isAnnotationPresent(cast EnumReflectionInformation);
	}

	static function isEnumValueClass<T>(c:java.lang.Class<T>):Bool {
		// TODO: have to be careful if we ever decide to omit EnumValueReflectionInformation
		// Maybe a separate HaxeEnum annotation would be better here
		return c.isAnnotationPresent(cast EnumValueReflectionInformation);
	}

	public static function getClass<T>(o:T):Null<Class<T>> {
		if (o == null) {
			return null;
		}
		if (Jvm.instanceof(o, Class)) {
			return null;
		}
		var c = o.object().getClass();
		if (isEnumValueClass(c)) {
			return null;
		}
		if (c == jvm.DynamicObject.native() || Jvm.instanceof(o, jvm.DynamicObject)) {
			return null;
		}
		return c.haxe();
	}

	public static function getEnum(o:EnumValue):Enum<Dynamic> {
		if (o == null) {
			return null;
		}
		var c = o.object().getClass().getSuperclass();
		if (!c.isAnnotationPresent(cast EnumReflectionInformation)) {
			return null;
		}
		return c.haxeEnum();
	}

	public static function getSuperClass(c:Class<Dynamic>):Class<Dynamic> {
		if (c == String) {
			return null;
		}
		var c = c.native();
		var cSuper = c.getSuperclass();
		if (cSuper == null) {
			return null;
		}
		var annotation = c.getAnnotation((cast ClassReflectionInformation : java.lang.Class<ClassReflectionInformation>));
		if (annotation != null && annotation.hasSuperClass() == false) {
			return null;
		}
		return cSuper.haxe();
	}

	public static function getClassName(c:Class<Dynamic>):String {
		return switch (c.native().getName()) {
			case "java.lang.String": "String";
			case "java.lang.Math": "Math";
			case s if (s.indexOf("haxe.root.") == 0): s.substr(10);
			case s: s;
		}
	}

	public static function getEnumName(e:Enum<Dynamic>):String {
		return switch e.native().getName() {
			case s if(s.indexOf("haxe.root.") == 0): s.substr(10);
			case s: s;
		}
	}

	public static function resolveClass(name:String):Class<Dynamic> {
		if (name.indexOf(".") == -1) {
			name = "haxe.root." + name;
		}
		return try {
			java.lang.Class.forName(name).haxe();
		} catch (e:java.lang.ClassNotFoundException) {
			return switch (name) {
				case "haxe.root.String": java.NativeString;
				case "haxe.root.Math": java.lang.Math;
				case _: null;
			}
		}
	}

	public static function resolveEnum(name:String):Enum<Dynamic> {
		if (name.indexOf(".") == -1) {
			name = "haxe.root." + name;
		}
		return try {
			var c = java.lang.Class.forName(name);
			if (!isEnumClass(c)) {
				null;
			} else {
				c.haxeEnum();
			}
		} catch (e:java.lang.ClassNotFoundException) {
			return null;
		}
	}

	static final emptyArg = (null : jvm.EmptyConstructor);

	static final emptyClass = jvm.EmptyConstructor.native();

	public static function createInstance<T>(cl:Class<T>, args:Array<Dynamic>):T {
		var args = @:privateAccess args.getNative();
		var cl = cl.native();
		var ctors = cl.getConstructors();
		var emptyCtor:Null<java.lang.reflect.Constructor<T>> = null;
		// 1. Look for real constructor. If we find the EmptyConstructor constructor, store it
		for (ctor in ctors) {
			var params = ctor.getParameterTypes();
			if (params.length == 1 && params[0] == jvm.EmptyConstructor.native()) {
				emptyCtor = cast ctor;
				continue;
			}
			switch (Jvm.unifyCallArguments(args, params, true)) {
				case Some(args):
					ctor.setAccessible(true);
					return ctor.newInstance(...args);
				case None:
			}
		}
		// 2. If there was the EmptyConstructor constructor, look for a matching new method
		if (emptyCtor != null) {
			var methods = cl.getMethods();
			for (method in methods) {
				if (method.getName() != "new") {
					continue;
				}
				var params = method.getParameterTypes();
				switch (Jvm.unifyCallArguments(args, params, true)) {
					case Some(args):
						var obj = emptyCtor.newInstance(emptyArg);
						method.setAccessible(true);
						method.invoke(obj, ...args);
						return obj;
					case None:
				}
			}
		}
		return null;
	}

	public static function createEmptyInstance<T>(cl:Class<T>):T {
		var annotation = (cl.native().getAnnotation((cast ClassReflectionInformation : java.lang.Class<ClassReflectionInformation>)));
		if (annotation != null) {
			return cl.native().getConstructor(emptyClass)
				.newInstance(emptyArg);
		} else {
			return cl.native().newInstance();
		}
	}

	public static function createEnum<T>(e:Enum<T>, constr:String, ?params:Array<Dynamic>):T {
		if (params == null || params.length == 0) {
			var v:Dynamic = Jvm.readField(e, constr);
			if (!Std.isOfType(v, e)) {
				throw 'Could not create enum value ${getEnumName(e)}.$constr: Unexpected value $v';
			}
			return v;
		} else {
			return Reflect.callMethod(null, Jvm.readField(e, constr), params);
		}
	}

	public static function createEnumIndex<T>(e:Enum<T>, index:Int, ?params:Array<Dynamic>):T {
		var clInfo:java.lang.Class<EnumReflectionInformation> = cast EnumReflectionInformation;
		var annotation = e.native().getAnnotation(clInfo);
		if (params == null || params.length == 0) {
			return Jvm.readField(e, annotation.constructorNames()[index]);
		} else {
			return Reflect.callMethod(null, Jvm.readField(e, annotation.constructorNames()[index]), params);
		}
	}

	static function getFields<T>(c:java.lang.Class<T>, statics:Bool):Array<String> {
		var ret = [];
		for (f in c.getDeclaredFields()) {
			if (java.lang.reflect.Modifier.isStatic(f.getModifiers()) == statics && !f.isSynthetic()) {
				ret.push(f.getName());
			}
		}
		for (m in c.getDeclaredMethods()) {
			if (java.lang.reflect.Modifier.isStatic(m.getModifiers()) == statics && !m.isSynthetic()) {
				ret.push(m.getName());
			}
		}
		return ret;
	}

	public static function getInstanceFields(c:Class<Dynamic>):Array<String> {
		var fields = [];
		while (c != null) {
			for (field in fields.concat(getFields(c.native(), false))) {
				if (fields.indexOf(field) == -1) {
					fields.push(field);
				}
			}
			c = getSuperClass(c);
		};
		return fields;
	}

	public static function getClassFields(c:Class<Dynamic>):Array<String> {
		return getFields(c.native(), true);
	}

	public static function getEnumConstructs(e:Enum<Dynamic>):Array<String> {
		var clInfo:java.lang.Class<EnumReflectionInformation> = cast EnumReflectionInformation;
		var annotation = e.native().getAnnotation(clInfo);
		if (annotation != null) {
			return @:privateAccess Array.ofNative(annotation.constructorNames());
		}
		var vals = e.values();
		var ret = [];
		for (i in 0...vals.length) {
			ret[i] = vals[i].name();
		}
		return ret;
	}

	public static function typeof(v:Dynamic):ValueType {
		// could optimize this with an annotation on Haxe classes
		if (v == null) {
			return TNull;
		}
		if (Jvm.instanceof(v, java.lang.Number)) {
			var v:java.lang.Number = cast v;
			if (v.intValue() == v.doubleValue()) {
				return TInt;
			}
			return TFloat;
		}
		if (Jvm.instanceof(v, java.lang.Boolean.BooleanClass)) {
			return TBool;
		}
		if (Jvm.instanceof(v, jvm.DynamicObject)) {
			return TObject;
		}
		if (Jvm.instanceof(v, jvm.Function)) {
			return TFunction;
		}
		var c = (cast v : java.lang.Object).getClass();
		// TODO: native enums?
		if (isEnumValueClass(c)) {
			return TEnum(c.getSuperclass().haxeEnum());
		}
		if (Jvm.instanceof(v, java.lang.Class)) {
			return TObject;
		}
		return TClass(c.haxe());
	}

	public static function enumEq<T>(a:T, b:T):Bool {
		if (a == null) {
			return b == null;
		}
		var a:jvm.Enum<Dynamic> = cast a;
		var b:jvm.Enum<Dynamic> = cast b;
		return a.equals(b);
	}

	public static function enumConstructor(e:EnumValue):String {
		return (cast e : java.lang.Enum<Dynamic>).name();
	}

	public static function enumParameters(e:EnumValue):Array<Dynamic> {
		var a = (cast e : jvm.Enum<Dynamic>)._hx_getParameters();
		return @:privateAccess Array.ofNative(a);
	}

	public static function enumIndex(e:EnumValue):Int {
		return (cast e : java.lang.Enum<Dynamic>).ordinal();
	}

	public static function allEnums<T>(e:Enum<T>):Array<T> {
		var all = getEnumConstructs(e);
		var ret = [];
		for (name in all) {
			var v = Jvm.readField(e, name);
			if (Jvm.instanceof(v, java.lang.Enum)) {
				ret.push(v);
			}
		}
		return ret;
	}
}
