package unit.issues;

import haxe.ds.StringMap;
import haxe.ds.Vector;

private typedef MyVector = Vector<MyFunction>;
private typedef MyMap = StringMap<MyFunction>;
private typedef MyFunction = MyVector->MyMap->Dynamic;

class Issue3024 extends Test {

	private var myVector:MyVector;

	private var myMap:MyMap;

	function test() {
		create(new Vector(1), new StringMap());
	}

	function create(myVector:MyVector, myMap:MyMap) {
		this.myVector = myVector;
		this.myMap = myMap;
	}
}