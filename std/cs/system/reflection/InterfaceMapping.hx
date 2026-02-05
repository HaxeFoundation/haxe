package cs.system.reflection;

/** Retrieves the mapping of an interface into the actual methods on a class that implements that interface. */
@:native("System.Reflection.InterfaceMapping")
extern class InterfaceMapping extends cs.system.ValueType {
	/** Shows the methods that are defined on the interface. */
	var InterfaceMethods:cs.NativeArray<cs.system.reflection.MethodInfo>;
	/** Shows the type that represents the interface. */
	var InterfaceType:cs.system.Type;
	/** Shows the methods that implement the interface. */
	var TargetMethods:cs.NativeArray<cs.system.reflection.MethodInfo>;
	/** Represents the type that was used to create the interface mapping. */
	var TargetType:cs.system.Type;
}
