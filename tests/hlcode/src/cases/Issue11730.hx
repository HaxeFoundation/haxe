package cases;

private abstract DataRaw(haxe.DynamicAccess<Any>) to haxe.DynamicAccess<Any> {
	public var mappings(get, never):String;

	inline function get_mappings():String
		return this.get('mappings');
}

class Issue11730 {
	@:hl(<>
		fun@N(Nh) (dyn):hl.types.ArrayObj
		r0 dyn
		r1 hl.types.ArrayObj
		r2 dyn
		r3 String
		r4 String
		@0 global 3, $0
		@1 call 2, Reflect.field(0,3)
		@2 safecast 3,2
		@3 nullcheck 3
		@4 global 4, $1
		@5 call 1, String.split(3,4)
		@6 ret 1
	</>)
	static public function test(rawData:DataRaw) {
		return rawData.mappings.split(';');
	}
}
