import java.lang.invoke.*;
import java.lang.NoSuchMethodException;
import jvm.annotation.*;
import jvm.Jvm;

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

	public static function getClass<T>(o:T):Class<T> {
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
		return e.native().getName();
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

	static final emptyArg = {
		var a = new java.NativeArray(1);
		a[0] = (null : jvm.EmptyConstructor);
		a;
	}

	static final emptyClass = {
		var a = new java.NativeArray(1);
		a[0] = jvm.EmptyConstructor.native();
		a;
	}

	public static function createInstance<T>(cl:Class<T>, args:Array<Dynamic>):T {
		var args = @:privateAccess args.getNative();
		var cl = cl.native();
		var argTypes = Jvm.getArgumentTypes(args);
		var methodType = MethodType.methodType(cast Void, argTypes);
		// 1. attempt: direct constructor lookup
		try {
			var ctor = MethodHandles.lookup().findConstructor(cl, methodType);
			return ctor.invokeWithArguments(args);
		} catch (_:NoSuchMethodException) {}

		// 2. attempt direct new lookup
		try {
			var ctor = MethodHandles.lookup().findVirtual(cl, "new", methodType);
			var obj = cl.getConstructor(emptyClass).newInstance(emptyArg);
			ctor.bindTo(obj).invokeWithArguments(args);
			return obj;
		} catch (_:NoSuchMethodException) {}

		// 3. attempt: unify actual constructor
		for (ctor in cl.getDeclaredConstructors()) {
			switch (Jvm.unifyCallArguments(args, ctor.getParameterTypes())) {
				case Some(args):
					return MethodHandles.lookup().unreflectConstructor(ctor).invokeWithArguments(args);
				case None:
			}
		}

		// 4. attempt: unify new
		for (ctor in cl.getDeclaredMethods()) {
			if (ctor.getName() != "new") {
				continue;
			}
			switch (Jvm.unifyCallArguments(args, ctor.getParameterTypes())) {
				case Some(args):
					return MethodHandles.lookup().unreflect(ctor).invokeWithArguments(args);
				case None:
			}
		}

		return null;
	}

	public static function createEmptyInstance<T>(cl:Class<T>):T {
		var annotation = (cl.native().getAnnotation((cast ClassReflectionInformation : java.lang.Class<ClassReflectionInformation>)));
		if (annotation != null) {
			return cl.native().getConstructor(emptyClass).newInstance(emptyArg);
		} else {
			return cl.native().newInstance();
		}
	}

	public static function createEnum<T>(e:Enum<T>, constr:String, ?params:Array<Dynamic>):T {
		if (params == null || params.length == 0) {
			var v:Dynamic = Jvm.readField(e, constr);
			if (!Std.is(v, e)) {
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
		return @:privateAccess Array.ofNative(annotation.constructorNames());
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
		if (Jvm.instanceof(v, java.lang.invoke.MethodHandle)) {
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
		if (b == null) {
			return false;
		}
		var a:jvm.Enum<Dynamic> = cast a;
		var b:jvm.Enum<Dynamic> = cast b;
		if (a.ordinal() != b.ordinal()) {
			return false;
		}
		var params1 = a._hx_getParameters();
		var params2 = b._hx_getParameters();
		if (params1.length != params2.length) {
			return false;
		}
		for (i in 0...params1.length) {
			if (params1[i] != params2[i]) {
				if (Jvm.instanceof(params1[i], jvm.Enum)) {
					if (!enumEq(params1[i], params2[i])) {
						return false;
					}
				} else {
					return false;
				}
			}
		}
		return true;
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
			if (Jvm.instanceof(v, jvm.Enum)) {
				ret.push(v);
			}
		}
		return ret;
	}
}
