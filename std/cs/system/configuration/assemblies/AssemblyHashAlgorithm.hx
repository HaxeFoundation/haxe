package cs.system.configuration.assemblies;

/** Specifies all the hash algorithms used for hashing files and for generating the strong name. */
@:native("System.Configuration.Assemblies.AssemblyHashAlgorithm")
extern enum abstract AssemblyHashAlgorithm(Int) {
	var MD5 = 32771;
	var None = 0;
	var SHA1 = 32772;
	var SHA256 = 32780;
	var SHA384 = 32781;
	var SHA512 = 32782;
}
