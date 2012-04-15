package haxe.lang;

/**
 * ...
 * @author waneck
 */

@:native("cs.native.Exception") @:nativegen extern class Exception
{
	public var message:String;
	
	private function new():Void;
}

@:native("cs.native.NullArgumentException") @:nativegen extern class NullArgumentException extends Exception
{
	public function new():Void;
}

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
	
	public function toString()
	{
		return "Haxe Exception: " + obj;
	}
	
	public static function wrap(obj:Dynamic):Exception
	{
		if (Std.is(obj, Exception)) return obj;
		
		return new HaxeException(obj);
	}
}