package php7.reflection;


@:native('ReflectionProperty')
extern class ReflectionProperty implements Reflector {
    @:phpClassConst static var IS_STATIC : Int;
    @:phpClassConst static var IS_PUBLIC : Int;
    @:phpClassConst static var IS_PROTECTED : Int;
    @:phpClassConst static var IS_PRIVATE : Int;

    public var name : String;

    public static function export( className:Dynamic, name:String, ?returnValue:Bool ) : String;

    public function new ( cls:Dynamic, name:String ) : Void;
    public function getDeclaringClass() : ReflectionClass;
    public function getDocComment() : String;
    public function getModifiers() : Int;
    public function getName() : String;
    public function getValue( ?object:{} ) : Dynamic;
    public function isPrivate() : Bool;
    public function isProtected() : Bool;
    public function isPublic() : Bool;
    public function isStatic() : Bool;
    public function setAccessible ( accessible:Bool ) : Void;
    public function setValue( object:{}, value:Dynamic ) : Void;
    public function __toString() : String;
}