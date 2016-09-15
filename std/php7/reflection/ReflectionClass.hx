package php7.reflection;

import haxe.extern.Rest;

@:native('ReflectionClass')
extern class ReflectionClass implements Reflector {    
    // const integer IS_IMPLICIT_ABSTRACT = 16 ;
    // const integer IS_EXPLICIT_ABSTRACT = 32 ;
    // const integer IS_FINAL = 64 ;
    
    public static function export( argument:Dynamic, returnValue:Bool = false ) : String;

    public var name : String;
        
    public function new( argument:Dynamic ) : Void;    
    public function getConstant( name:String ) : Dynamic;
    public function getConstants() : NativeAssocArray<Dynamic>;
    // public function getConstructor() : ReflectionMethod;
    public function getDefaultProperties() : NativeAssocArray<Dynamic>;
    public function getDocComment() : String;
    public function getEndLine() : Int;
    // public function getExtension() : ReflectionExtension;
    public function getExtensionName() : String;
    public function getFileName() : String;
    public function getInterfaceNames() : NativeIndexedArray<String>;
    // public function getInterfaces() : NativeIndexedArray<ReflectionClass>;
    // public function getMethod( name:String ) : ReflectionMethod;
    // public function getMethods( ?filter:Int ) : NativeIndexedArray<ReflectionMethod>;
    public function getModifiers() : Int;
    public function getName() : String;
    public function getNamespaceName() : String;
    // public function getParentClass() : Null<ReflectionClass>;
    // public function getProperties( ?filter:Int ) : NativeIndexedArray<ReflectionProperty>;
    // public function getProperty( name:String ) : ReflectionProperty;
    public function getShortName() : String;
    public function getStartLine() : Int;
    public function getStaticProperties() : NativeAssocArray<Dynamic>;
    public function getStaticPropertyValue( name:String, ?def_value:Ref<Dynamic> ) : Dynamic;
    public function getTraitAliases() : NativeAssocArray<String>;
    public function getTraitNames() : NativeIndexedArray<String>;
    // public function getTraits() : NativeIndexedArray<ReflectionClass>;
    public function hasConstant( name:String ) : Bool;
    public function hasMethod( name:String ) : Bool;
    public function hasProperty( name:String ) : Bool;
    public function implementsInterface( interfaceName:String ) : Bool;
    public function inNamespace() : Bool;
    public function isAbstract() : Bool;
    public function isAnonymous() : Bool;
    public function isCloneable() : Bool;
    public function isFinal() : Bool;
    public function isInstance( object:{} ) : Bool;
    public function isInstantiable() : Bool;
    public function isInterface() : Bool;
    public function isInternal() : Bool;
    public function isIterateable() : Bool;
    public function isSubclassOf( className:String ) : Bool;
    public function isTrait() : Bool;
    public function isUserDefined() : Bool;
    public function newInstance( args:Rest<Dynamic> ) : Dynamic;
    public function newInstanceArgs( ?args:NativeIndexedArray<Dynamic> ) : Dynamic;
    public function newInstanceWithoutConstructor() : Dynamic;
    public function setStaticPropertyValue( name:String , value:String ) : Void;
    public function __toString() : String;
}