package jvm.annotation;

@:annotation
@:native("haxe.jvm.annotation.EnumValueReflectionInformation")
@:keep
interface EnumValueReflectionInformation extends java.lang.annotation.Annotation {
	function argumentNames():java.NativeArray<String>;
}
