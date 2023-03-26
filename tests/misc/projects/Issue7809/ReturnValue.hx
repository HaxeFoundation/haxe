abstract ReturnValue(Int) from Int {
	public function new(i:Int):Void {
		this = i;
		return 123;
	}
}