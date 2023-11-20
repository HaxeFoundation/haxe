@:annotation("RUNTIME")
interface MyVisibleAnnotation {}

@:annotation("CLASS")
interface MyInvisibleAnnotation {}

@:annotation("RUNTIME")
interface MyVisibleArrayAnnotation {
	function value():java.NativeArray<String>;
}

@:strict(MyVisibleAnnotation())
@:strict(MyInvisibleAnnotation())
@:strict(MyVisibleArrayAnnotation({value: ["foo", "bar"]}))
class AnnotationLib {
	@:strict(MyVisibleAnnotation())
	@:strict(MyInvisibleAnnotation())
	static function test(@:strict(MyVisibleAnnotation()) arg1:String, @:strict(MyInvisibleAnnotation()) arg2:String) {}
}
