package cs.internal;

@:native('haxe.lang.Iterator') interface Iterator<T>
{
	
	public function hasNext():Bool;
	
	public function next():T;
	
}

@:native('haxe.lang.Iterable') interface Iterable<T>
{
	
	public function iterator():Iterator<T>;
	
}