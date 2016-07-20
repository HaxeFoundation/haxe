package unit.issues;
class Issue5248 extends Test{
	function test(){
		var f1 = new Issue5248Foo();
		eq(f1.saved, "123");
	}
}

class Issue5248Foo {
    var processData:Array<Int>->Array<Int>;
    public var saved : String; 


    public function new():Void {
        var arr = [1,2,3];
        this.processData = _processData;
        saved = processData(arr).join("");
    }

    function _processData(arr:Array<Int>):Array<Int> {
        return arr;
    }
}
