package haxe.lang;

/**
 * ...
 * @author waneck
 */

interface Iterator<T>
{
	
	public function hasNext():Bool;
	
	public function next():T;
	
}

interface Iterable<T>
{
	
	public function iterator():Iterator<T>;
	
}