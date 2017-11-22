package php.reflection;


@:native('ReflectionProperty')
extern class ReflectionProperty implements Reflector {
    @:phpClassConst static var IS_STATIC : Int;
    @:phpClassConst static var IS_: Int;
    @:phpClassConst static var IS_PROTECTED : Int;
    @:phpClassConst static var IS_PRIVATE : Int;

    var name : String;

    static function export( className:Dynamic, name:String, ?returnValue:Bool ) : String;

    function new ( cls:Dynamic, name:String ) : Void;
    function getDeclaringClass() : ReflectionClass;
    function getDocComment() : String;
    function getModifiers() : Int;
    function getName() : String;
    function getValue( ?object:{} ) : Dynamic;
    function isPrivate() : Bool;
    function isProtected() : Bool;
    function isPublic() : Bool;
    function isStatic() : Bool;
    function setAccessible ( accessible:Bool ) : Void;
    function setValue( object:{}, value:Dynamic ) : Void;
    @:phpMagic function __toString() : String;
}