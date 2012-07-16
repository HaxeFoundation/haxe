package cs.system;

@:native("System.Exception") @:nativegen extern class Exception
{
	public var message:String;
	
	private function new():Void;
}