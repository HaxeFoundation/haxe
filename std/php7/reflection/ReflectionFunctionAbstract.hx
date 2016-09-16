package php7.reflection;


@:native('ReflectionFunctionAbstract')
extern class ReflectionFunctionAbstract implements Reflector {
    
    public var name : String;
    
    public function getClosureScopeClass() : ReflectionClass;
    public function getClosureThis() : Dynamic;
    public function getDocComment() : String;
    public function getEndLine() : Int;
    // public function getExtension() : ReflectionExtension;
    public function getExtensionName() : String;
    public function getFileName() : String;
    public function getName() : String;
    public function getNamespaceName() : String;
    public function getNumberOfParameters() : Int;
    public function getNumberOfRequiredParameters() : Int;
    // public function getParameters() : NativeIndexedArray<ReflectionParameter>;
    // public function getReturnType() : ReflectionType;
    public function getShortName() : String;
    public function getStartLine() : Int;
    public function getStaticVariables() : NativeAssocArray<Dynamic>;
    public function hasReturnType() : Bool;
    public function inNamespace() : Bool;
    public function isClosure() : Bool;
    public function isDeprecated() : Bool;
    public function isGenerator() : Bool;
    public function isInternal() : Bool;
    public function isUserDefined() : Bool;
    public function isVariadic() : Bool;
    public function returnsReference() : Bool;
    public function __toString() : String;
}