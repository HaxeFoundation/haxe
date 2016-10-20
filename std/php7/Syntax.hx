package php7;

import haxe.extern.Rest;
import haxe.extern.EitherType;

/**
    Special extern class to support PHP language specifics.
    Don't use these functions unless you are really sure what you are doing.
**/
extern class Syntax {
    /**
        `==` operator in Haxe uses strict equality check.
        This method allows to compare values with non-strict equality check.
    **/
    static function equal( value1:Dynamic, value2:Dynamic ) : Bool;

    /**
        Generates `(int)$value`
    **/
    static function int( value:Dynamic ) : Int;

    /**
        Generates `(float)$value`
    **/
    static function float( value:Dynamic ) : Float;

    /**
        Generates `(string)$value`
    **/
    static function string( value:Dynamic ) : String;

    /**
        Generates `(bool)$value`
    **/
    static function bool( value:Dynamic ) : Bool;

    /**
        Generates `(object)$value`
    **/
    static function object( value:Dynamic ) : Dynamic;

    /**
        Generates `(array)$value`
    **/
    static function array( value:Dynamic ) : NativeArray;

    /**
        Ggenerates `$value instanceof $phpClassName`.
        `type` only accepts direct class names. That means `Type.resolveClass('MyClass')` is not allowed, but `MyClass` is.
    **/
    static function instanceof( value:Dynamic,  type:Class<Dynamic> ) : Bool;

    /**
        ```
        PHP.foreach(collection, function(key, value) trace(key, value));
        ```
        generates:
        ```
        foreach($collection as $key => $value) {
            trace($key, $value);
        }
        ```
    **/
    static function foreach<TKey,TValue>( collection:Dynamic, body:TKey->TValue->Void ) : Void;

    /**
        Generates `new $className($arg1, ...$argN)`
    **/
    static function construct( className:String, args:Rest<Dynamic>) : Dynamic;

    /**
        Generates instance field access for reading on `object`
    **/
    static function getField( object:Dynamic, fieldName:String ) : Dynamic;

    /**
        Generates instance field access for writing on `object`
    **/
    static function setField( object:Dynamic, fieldName:String, value:Dynamic ) : Void;

    /**
        Generates a call to instance method: `$object->{$methodName}(<args>)`
    **/
    static function call( object:Dynamic, methodName:String, args:Rest<Dynamic> ) : Dynamic;

    /**
        ```
        PHP.arrayDecl(arg1, arg2, arg3);
        ```
        Generates native array declaration:
        ```
        [$arg1, $arg2, $arg3]
        ```
    **/
    static function arrayDecl( args:Rest<Dynamic> ) : NativeIndexedArray<Dynamic>;

    /**
        Don't let compiler to optimize away local var passed to this method.
    **/
    static function keepVar( localVar:Dynamic ) : Void;

    static function splat( args:EitherType<NativeArray, Traversable> ) : Rest<Dynamic>;
}