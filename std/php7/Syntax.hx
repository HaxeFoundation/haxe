package php;

import haxe.extern.Rest;
import haxe.extern.AsVar;
import haxe.extern.EitherType;

/**
    Special extern class to support PHP language specifics.
    Don't use these functions unless you are really sure what you are doing.
**/
extern class Syntax {
    /**
        This method allows to force specified binary operation for `left` and `right` values.
        `operator` must be a constant string like "+" or "==".
    **/
    static function binop( left:Dynamic, operator:String, right:Dynamic ) : Dynamic;

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
        Haxe generates `Std.is(value, Type)` calls to `$value instanceof Type` automatically where possible.
        `type` only accepts direct class names. That means `Type.resolveClass('MyClass')` is not allowed, but `MyClass` is.
    **/
    static function instanceof<V,C>( value:AsVar<V>,  type:AsVar<Class<C>> ) : Bool;

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
    static function foreach<TCollection,TKey,TValue>( collection:TCollection, body:TKey->TValue->Void ) : Void;

    /**
        Generates `new $className($arg1, ...$argN)`
    **/
    static function construct( className:AsVar<String>, args:Rest<Dynamic>) : Dynamic;

    /**
        Generates instance field access for reading on `object`
    **/
    static function getField<T>( object:AsVar<T>, fieldName:AsVar<String> ) : Dynamic;

    /**
        Generates instance field access for writing on `object`
    **/
    static function setField<T>( object:AsVar<T>, fieldName:AsVar<String>, value:Dynamic ) : Void;

    /**
        Generates static field access for reading on `className`
    **/
    static function getStaticField( className:AsVar<EitherType<Class<Dynamic>,String>>, fieldName:AsVar<String> ) : Dynamic;

    /**
        Generates static field access for writing on `object`
    **/
    static function setStaticField( object:AsVar<EitherType<Class<Dynamic>,String>>, fieldName:AsVar<String>, value:Dynamic ) : Void;

    /**
        Generates a call to instance method: `$object->{$methodName}(<args>)`
    **/
    static function call<T>( object:AsVar<T>, methodName:AsVar<String>, args:Rest<Dynamic> ) : Dynamic;
    /**
        Generates a call to static method: `$className::{$methodName}(<args>)`
    **/
    static function staticCall( className:AsVar<EitherType<Class<Dynamic>,String>>, methodName:AsVar<String>, args:Rest<Dynamic> ) : Dynamic;

    /**
        ```
        PHP.arrayDecl(arg1, arg2, arg3);
        ```
        Generates native array declaration:
        ```
        [$arg1, $arg2, $arg3]
        ```
    **/
    static function arrayDecl<T>( args:Rest<T> ) : NativeIndexedArray<T>;

    /**
        Don't let compiler to optimize away local var passed to this method.
    **/
    static function keepVar( localVar:Dynamic ) : Void;

    /**
        Adds `...` operator before `args`
    **/
    static function splat( args:EitherType<NativeArray, Traversable> ) : Rest<Dynamic>;

    /**
        Add errors suppression operator `@` before `expression`
    **/
    static function suppress<T>( expression:T ) : T;
}