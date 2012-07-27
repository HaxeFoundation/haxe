package cpp;

#if haxe3
@:generic
#end
extern class FastIterator<T> #if !haxe3 implements haxe.rtti.Generic #end
{
	public function hasNext():Bool;
	public function next():T;
}

