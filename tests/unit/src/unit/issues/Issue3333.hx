package unit.issues;

private abstract MyString(String) to String {
    public function new (url:String) this = url;
	public inline function get() return this;
}

class Issue3333 extends Test {
	function test() {
        var f = MyString.new.bind("hey");
		eq("hey", f().get());
	}
}