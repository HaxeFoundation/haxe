package cs.internal;
import cs.system.Exception;

//should NOT be usable inside haxe code
@:nativegen @:keep @:native("haxe.lang.HaxeException") private class HaxeException extends Exception
{
	private var obj:Dynamic;
	
	public function new(obj:Dynamic)
	{
		super();
		
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
	
	public static function wrap(obj:Dynamic):Exception
	{
		if (Std.is(obj, Exception)) return obj;
		
		return new HaxeException(obj);
	}
}