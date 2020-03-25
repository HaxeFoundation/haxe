class Main {
	static function main() {
		final base:NullChild = {};
		final base:BaseEmpty = {};
		final base:ChildEmpty = {};
		final base:Base = {};
		final child:Child = {base: 200, child: 100};
		final child:Child = {child: 100};
		final child:OptionalChild = {};
		final child:OptionalEmptyChild = {};
		final child:FatChild = {};
		final child:FatEmptyChild = {};
	}
}
@:structInit
class BaseNullEmpty {
	final base:Null<Int>;
}
@:structInit
class NullChild extends BaseNullEmpty {
	@:optional final child: Int;
}
@:structInit
class BaseEmpty {
	final base:Int;
}
@:structInit
class Base {
	final base = 0;
}
@:structInit
class OptionalChild extends Base {
	@:optional final child: Int;
}
@:structInit
class OptionalEmptyChild extends BaseEmpty {
	@:optional final child: Int;
}
@:structInit
class Child extends Base {
	final child: Int;
}
@:structInit
class ChildEmpty extends BaseEmpty {
	final child: Int;
}
@:structInit
class FatChild extends Child {
	final fatChild: Int;
}
@:structInit
class FatEmptyChild extends ChildEmpty {
	final fatChild: Int;
}
