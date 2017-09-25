package php.reflection;

import haxe.extern.Rest;

@:native('ReflectionClass')
extern class ReflectionClass implements Reflector {
    @:phpClassConst static var IS_IMPLICIT_ABSTRACT : Int;
    @:phpClassConst static var IS_EXPLICIT_ABSTRACT : Int;
    @:phpClassConst static var IS_FINAL : Int;

    static function export( argument:Dynamic, returnValue:Bool = false ) : String;

    var name : String;

    function new( argument:Dynamic ) : Void;
    function getConstant( name:String ) : Dynamic;
    function getConstants() : NativeAssocArray<Dynamic>;
    function getConstructor() : ReflectionMethod;
    function getDefaultProperties() : NativeAssocArray<Dynamic>;
    function getDocComment() : String;
    function getEndLine() : Int;
    // function getExtension() : ReflectionExtension;
    function getExtensionName() : String;
    function getFileName() : String;
    function getInterfaceNames() : NativeIndexedArray<String>;
    function getInterfaces() : NativeIndexedArray<ReflectionClass>;
    function getMethod( name:String ) : ReflectionMethod;
    function getMethods( ?filter:Int ) : NativeIndexedArray<ReflectionMethod>;
    function getModifiers() : Int;
    function getName() : String;
    function getNamespaceName() : String;
    function getParentClass() : Null<ReflectionClass>;
    function getProperties( ?filter:Int ) : NativeIndexedArray<ReflectionProperty>;
    function getProperty( name:String ) : ReflectionProperty;
    function getShortName() : String;
    function getStartLine() : Int;
    function getStaticProperties() : NativeAssocArray<Dynamic>;
    function getStaticPropertyValue( name:String, ?def_value:Ref<Dynamic> ) : Dynamic;
    function getTraitAliases() : NativeAssocArray<String>;
    function getTraitNames() : NativeIndexedArray<String>;
    function getTraits() : NativeIndexedArray<ReflectionClass>;
    function hasConstant( name:String ) : Bool;
    function hasMethod( name:String ) : Bool;
    function hasProperty( name:String ) : Bool;
    function implementsInterface( interfaceName:String ) : Bool;
    function inNamespace() : Bool;
    function isAbstract() : Bool;
    function isAnonymous() : Bool;
    function isCloneable() : Bool;
    function isFinal() : Bool;
    function isInstance( object:{} ) : Bool;
    function isInstantiable() : Bool;
    function isInterface() : Bool;
    function isInternal() : Bool;
    function isIterateable() : Bool;
    function isSubclassOf( className:String ) : Bool;
    function isTrait() : Bool;
    function isUserDefined() : Bool;
    function newInstance( args:Rest<Dynamic> ) : Dynamic;
    function newInstanceArgs( ?args:NativeIndexedArray<Dynamic> ) : Dynamic;
    function newInstanceWithoutConstructor() : Dynamic;
    function setStaticPropertyValue( name:String , value:String ) : Void;
    @:phpMagic function __toString() : String;
}