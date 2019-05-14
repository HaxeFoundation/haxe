package jvm.annotation;

@:annotation
@:native("haxe.jvm.annotation.EnumReflectionInformation")
@:keep
interface EnumReflectionInformation extends java.lang.annotation.Annotation {
	function constructorNames():java.NativeArray<String>;
}
