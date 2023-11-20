@:annotation("RUNTIME")
interface MyVisibleAnnotation {}

@:annotation("CLASS")
interface MyInvisibleAnnotation {}

@:annotation("RUNTIME")
interface MyVisibleArrayAnnotation {
	function value():java.NativeArray<String>;
}

@:annotation("RUNTIME")
interface MyVisibleArrayArrayAnnotation {
	function value():java.NativeArray<java.NativeArray<String>>;
}

@:strict(MyVisibleAnnotation())
@:strict(MyInvisibleAnnotation())
@:strict(MyVisibleArrayAnnotation({value: ["foo", "bar"]}))
@:strict(MyVisibleArrayArrayAnnotation({value: [["foo1", "bar1"], ["foo2", "bar2"]]}))
class AnnotationLib {
	@:strict(MyVisibleAnnotation())
	@:strict(MyInvisibleAnnotation())
	static function test(@:strict(MyVisibleAnnotation()) arg1:String, @:strict(MyInvisibleAnnotation()) arg2:String) {}
}
