package haxe.lang;
import java.lang.Throwable;

/**
 * ...
 * @author waneck
 */

@:nativegen @:keep @:native("haxe.lang.HaxeException") private class HaxeException extends RuntimeException
{
	private var obj:Dynamic;
	
	public function new(obj:Dynamic)
	{
		super(null, null);
		
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
		if (Std.is(obj, RuntimeException)) return obj;
		
		return new HaxeException(obj);
	}
}