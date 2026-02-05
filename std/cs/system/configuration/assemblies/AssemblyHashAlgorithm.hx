package cs.system.configuration.assemblies;

/** Specifies all the hash algorithms used for hashing files and for generating the strong name. */
@:native("System.Configuration.Assemblies.AssemblyHashAlgorithm")
extern enum AssemblyHashAlgorithm {
	MD5;
	None;
	SHA1;
	SHA256;
	SHA384;
	SHA512;
}
