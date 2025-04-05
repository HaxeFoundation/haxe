@:structInit
class Parent {
	/** This is y **/
	var y:String;
}

@:structInit
class Child extends Parent {
	/** This is x **/
	var x:Int;
}

function main() {
	var c:Child = {};
}