package unit.issues;

private abstract MyAbstract(Int) {
    public var inlineProp(get, set):Int;

    public function new() {this = 0;}

    private inline function get_inlineProp():Int {return this;}
    private inline function set_inlineProp(value:Int):Int {return this = value;}
}

class Issue3042 extends Test {
	function test() {
		var a = new MyAbstract();
		a.inlineProp += 2;
		eq(2, a.inlineProp);
	}
}