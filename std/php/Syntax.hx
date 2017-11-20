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
        Embeds plain php code.
        `php` should be a string literal with php code.
        It can contain placeholders like `{0}`, `{1}` which will be replaced with corresponding arguments from `args`.
        E.g.:
        ```
        Syntax.code("var_dump({0}, {1})", a, b);
        ```
        will generate
        ```
        var_dump($a, $b);
        ```
    **/
    static function code(php:String, args:Rest<Dynamic>):Dynamic;

    /**
        The same as `code()`, but adds dereferencing
        when required to workaround "cannot use temporary expression in write context" php error.
    **/
    static function codeDeref(php:String, args:Rest<Dynamic>):Dynamic;

    /**
        Generates `$left <=> $right`
    **/
    static inline function spaceship( left:Dynamic, right:Dynamic ) : Int {
        return code('({0} <=> {1})', left, right);
    }

    /**
        Generates `$left ?? $right`
    **/
    static inline function coalesce<T>( left:T, right:T ) : T {
        return codeDeref('({0} ?? {1})', left, right);
    }

    /**
        Generates `$left . $right`
    **/
    static inline function concat( left:String, right:String ) : String {
        return code('({0} . {1})', left, right);
    }

    /**
        Generates `$left == $right`
    **/
    static inline function equal( left:Dynamic, right:Dynamic ) : Bool {
        return code('({0} == {1})', left, right);
    }

    /**
        Generates `$left === $right`
    **/
    static inline function strictEqual( left:Dynamic, right:Dynamic ) : Bool {
        return code('({0} === {1})', left, right);
    }

    /**
        Generates `$left != $right`
    **/
    static inline function notEqual( left:Dynamic, right:Dynamic ) : Bool {
        return code('({0} != {1})', left, right);
    }

    /**
        Generates `$left !== $right`
    **/
    static inline function strictNotEqual( left:Dynamic, right:Dynamic ) : Bool {
        return code('({0} !== {1})', left, right);
    }

    /**
        Generates `$left + $right` for numbers.
    **/
    static inline function add<T:Float>( left:T, right:T ) : T {
        return code('({0} + {1})', left, right);
    }

    /**
        Generates `$left + $right` for php arrays.
    **/
    static inline function union( left:NativeArray, right:NativeArray ) : NativeArray {
        return codeDeref('({0} + {1})', left, right);
    }

    /**
        Generates `$left ** $right`
    **/
    static inline function exp<T:Float>( left:T, right:T ) : T {
        return code('({0} ** {1})', left, right);
    }

    /**
        Generates `$left ?: $right`
    **/
    static inline function shortTernary<T>( left:T, right:T ) : T {
        return codeDeref('({0} ?: {1})', left, right);
    }

    /**
        Generates `$left xor $right`
    **/
    static inline function xor( left:Bool, right:Bool ) : Bool {
        return code('({0} xor {1})', left, right);
    }

    /**
        Generates `(int)$value`
    **/
    static inline function int( value:Dynamic ) : Int {
        return code('(int)({0})', value);
    }

    /**
        Generates `(float)$value`
    **/
    static inline function float( value:Dynamic ) : Float {
        return code('(float)({0})', value);
    }

    /**
        Generates `(string)$value`
    **/
    static inline function string( value:Dynamic ) : String {
        return code('(string)({0})', value);
    }

    /**
        Generates `(bool)$value`
    **/
    static inline function bool( value:Dynamic ) : Bool {
        return code('(bool)({0})', value);
    }

    /**
        Generates `(object)$value`
    **/
    static inline function object( value:Dynamic ) : StdClass {
        return codeDeref('(object)({0})', value);
    }

    /**
        Generates `(array)$value`
    **/
    static inline function array( value:Dynamic ) : NativeArray {
        return codeDeref('(array)({0})', value);
    }

    /**
        Generates `$value instanceof $phpClassName`.
        Haxe generates `Std.is(value, Type)` calls as `$value instanceof Type` automatically where possible.
        So you may need this only if you have a `Class` stored in a variable.
    **/
    static function instanceof<V,C>( value:AsVar<V>,  type:AsVar<Class<C>> ) : Bool;

    /**
        ```
        Syntax.foreach(collection, function(key, value) trace(key, value));
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
        Syntax.arrayDecl(arg1, arg2, arg3);
        ```
        Generates native array declaration:
        ```
        [$arg1, $arg2, $arg3]
        ```
    **/
    static function arrayDecl<T>( args:Rest<T> ) : NativeIndexedArray<T>;

    /**
        ```
        Syntax.assocDecl({field1:'first', field2:2}});
        ```
        Generates native associative array declaration:
        ```
        ["field1" => "first", "field2" => 2];
        ```
        This method is not recursive.
        Accepts object declarations only.
        That means you can't pass an object stored in a variable to this method like `Syntax.assocDecl(someVar)`.
        Use `php.Lib.associativeArrayOfObject(someVar)` instead.
    **/
    static function assocDecl<T:{}>( ?arg:T ) : NativeAssocArray<Dynamic>;

    /**
        Don't let compiler to optimize away local var passed to this method.
    **/
    static function keepVar( localVar:Dynamic ) : Void;

    /**
        Adds `...` operator before `args`
    **/
    static inline function splat( args:EitherType<NativeArray, Traversable> ) : Rest<Dynamic> {
        return code('...{0}', args);
    }

    /**
        Add errors suppression operator `@` before `expression`
    **/
    static function suppress<T>( expression:T ) : T;
}
