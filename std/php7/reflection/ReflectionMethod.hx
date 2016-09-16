package php7.reflection;

import haxe.Constraints;
import haxe.extern.Rest;

@:native('ReflectionMethod')
extern class ReflectionMethod extends ReflectionFunctionAbstract {    
    // const integer IS_STATIC = 1 ;
    // const integer IS_PUBLIC = 256 ;
    // const integer IS_PROTECTED = 512 ;
    // const integer IS_PRIVATE = 1024 ;
    // const integer IS_ABSTRACT = 2 ;
    // const integer IS_FINAL = 4 ;
    
    // public var class : String;
    
    public static function export( className:String, name:String, ?returnValue:Bool) : String;

    public function new( cls:Dynamic, name:String ) : Void;   
    public function getClosure( object:{} ) : Function;
    public function getDeclaringClass() : ReflectionClass;
    public function getModifiers() : Int;
    public function getPrototype() : ReflectionMethod;
    public function invoke( object:{}, args:Rest<Dynamic> ) : Dynamic;
    public function invokeArgs( object:{}, args:NativeIndexedArray<Dynamic> ) : Dynamic;
    public function isAbstract() : Bool;
    public function isConstructor() : Bool;
    public function isDestructor() : Bool;
    public function isFinal() : Bool;
    public function isPrivate() : Bool;
    public function isProtected() : Bool;
    public function isPublic() : Bool;
    public function isStatic() : Bool;
    public function setAccessible( accessible:Bool ) : Void;
}