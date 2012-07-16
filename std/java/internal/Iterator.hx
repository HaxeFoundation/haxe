package java.internal;

@:native('haxe.lang.Iterator')
@:keep
interface Iterator<T>
{
	
	public function hasNext():Bool;
	
	public function next():T;
	
}

@:native('haxe.lang.Iterable')
@:keep
interface Iterable<T>
{
	
	public function iterator():Iterator<T>;
	
}