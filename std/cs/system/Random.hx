package cs.system;

@:native("System.Random")
extern class Random 
{

	public function new():Void;
	
	@:overload(function(max:Int):Int {})
	@:overload(function(min:Int, max:Int):Int {})
	public function Next():Int;
	
	public function NextDouble():Float;
}