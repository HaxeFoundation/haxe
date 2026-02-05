package cs.system.configuration.assemblies;

/** Defines the different types of assembly version compatibility. This feature is not available in version 1.0 of the .NET Framework. */
@:native("System.Configuration.Assemblies.AssemblyVersionCompatibility")
extern enum AssemblyVersionCompatibility {
	SameDomain;
	SameMachine;
	SameProcess;
}
