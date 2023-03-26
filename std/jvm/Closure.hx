package jvm;

import java.NativeArray;
import java.lang.reflect.Method;

@:native("haxe.jvm.Closure")
@:nativeGen
@:keep
class Closure extends ClosureDispatch {
	public var context:Dynamic;
	public var method:Method;

	var isStatic:Bool;
	var params:NativeArray<java.lang.Class<Dynamic>>;

	public function new(context:Null<Dynamic>, method:Method) {
		super();
		this.context = context;
		this.method = method;
		isStatic = method.getModifiers() & java.lang.reflect.Modifier.STATIC != 0;
		params = method.getParameterTypes();
	}

	public function bindTo(context:Dynamic) {
		return new Closure(context, method);
	}

	override public function equals(other:java.lang.Object) {
		if (!Jvm.instanceof(other, Closure)) {
			return false;
		}
		var other:Closure = cast other;
		return context == other.context && method == other.method;
	}

	public override function invokeDynamic(args:NativeArray<Dynamic>):Dynamic {
		if (isStatic && context != null) {
			var newArgs = new NativeArray(args.length + 1);
			haxe.ds.Vector.blit(cast args, 0, cast newArgs, 1, args.length);
			newArgs[0] = context;
			args = newArgs;
		}
		var args = switch (jvm.Jvm.unifyCallArguments(args, params, true)) {
			case Some(args):
				args;
			case None:
				args;
		};
		try {
			return method.invoke(context, ...args);
		} catch (e:java.lang.reflect.InvocationTargetException) {
			throw e.getCause();
		}
	}
}

@:native("haxe.jvm.ClosureDispatch")
extern class ClosureDispatch extends Function {}

@:native("haxe.jvm.VarArgs")
extern class VarArgs extends Function {
	var func:Function;

	public function new(func:Function):Void;
}
