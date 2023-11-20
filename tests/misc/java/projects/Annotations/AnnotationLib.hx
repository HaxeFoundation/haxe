@:annotation("RUNTIME")
class MyVisibleAnnotation {}

@:annotation("CLASS")
class MyInvisibleAnnotation {}

@:strict(MyVisibleAnnotation())
@:strict(MyInvisibleAnnotation())
class AnnotationLib {
	@:strict(MyVisibleAnnotation())
	@:strict(MyInvisibleAnnotation())
	static function test(@:strict(MyVisibleAnnotation()) arg1:String, @:strict(MyInvisibleAnnotation()) arg2:String) {}
}
