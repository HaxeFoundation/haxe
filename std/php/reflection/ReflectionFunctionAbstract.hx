package php.reflection;


@:native('ReflectionFunctionAbstract')
extern class ReflectionFunctionAbstract implements Reflector {

    var name : String;

    function getClosureScopeClass() : ReflectionClass;
    function getClosureThis() : Dynamic;
    function getDocComment() : String;
    function getEndLine() : Int;
    // function getExtension() : ReflectionExtension;
    function getExtensionName() : String;
    function getFileName() : String;
    function getName() : String;
    function getNamespaceName() : String;
    function getNumberOfParameters() : Int;
    function getNumberOfRequiredParameters() : Int;
    // function getParameters() : NativeIndexedArray<ReflectionParameter>;
    // function getReturnType() : ReflectionType;
    function getShortName() : String;
    function getStartLine() : Int;
    function getStaticVariables() : NativeAssocArray<Dynamic>;
    function hasReturnType() : Bool;
    function inNamespace() : Bool;
    function isClosure() : Bool;
    function isDeprecated() : Bool;
    function isGenerator() : Bool;
    function isInternal() : Bool;
    function isUserDefined() : Bool;
    function isVariadic() : Bool;
    function returnsReference() : Bool;
    @:phpMagic function __toString() : String;
}