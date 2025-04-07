function main() {
	new Foo();
	new Foo("hello");
	new Foo(0);
}

extern class Foo {
	overload function new():Void;
	overload function new(s:String):Void;
	overload function new(i:Int):Void;
}