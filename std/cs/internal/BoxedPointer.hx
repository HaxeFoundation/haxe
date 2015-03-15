package cs.internal;

@:unsafe @:keep @:native('haxe.lang.BoxedPointer') @:nativeGen class BoxedPointer
{
	@:readonly public var value(default,null):Pointer<Dynamic>;

	public function new(val)
	{
		this.value = val;
	}

}
