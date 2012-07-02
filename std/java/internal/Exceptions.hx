package java.internal;
import java.lang.Throwable;

@:nativegen @:keep @:native("haxe.lang.HaxeException") private class HaxeException extends RuntimeException
{
	private var obj:Dynamic;
	
	public function new(obj:Dynamic, msg:String, cause:Throwable)
	{
		super(msg, cause);
		
		if (Std.is(obj, HaxeException))
		{
			var _obj:HaxeException = cast obj;
			obj = _obj.getObject();
		}
		
		this.obj = obj;
	}
	
	public function getObject():Dynamic
	{
		return obj;
	}
	
	public function toString():String
	{
		return "Haxe Exception: " + obj;
	}
	
	public static function wrap(obj:Dynamic):RuntimeException
	{
		if (Std.is(obj, RuntimeException)) 
			return obj;
		
		if (Std.is(obj, String))
			return new HaxeException(obj, obj, null);
		else if (Std.is(obj, Throwable))
			return new HaxeException(obj, null, obj);
		
		return new HaxeException(obj, null, null);
	}
}