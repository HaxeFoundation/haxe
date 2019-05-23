package jvm.annotation;

@:annotation
@:native("haxe.jvm.annotation.ClassReflectionInformation")
@:keep
interface ClassReflectionInformation extends java.lang.annotation.Annotation {
	function hasSuperClass():Bool;
}
