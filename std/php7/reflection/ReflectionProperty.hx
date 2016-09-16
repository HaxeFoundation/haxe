package php7.reflection;


@:native('ReflectionProperty')
extern class ReflectionProperty implements Reflector {
    
    // const integer IS_STATIC = 1 ;
    // const integer IS_PUBLIC = 256 ;
    // const integer IS_PROTECTED = 512 ;
    // const integer IS_PRIVATE = 1024 ;
        
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