package cs.system.data.common;

/** Represents a set of static methods for creating one or more instances of  classes. */
@:native("System.Data.Common.DbProviderFactories")
extern class DbProviderFactories {
	@:overload(function(connection:cs.system.data.common.DbConnection):cs.system.data.common.DbProviderFactory {})
	@:overload(function(providerRow:cs.system.data.DataRow):cs.system.data.common.DbProviderFactory {})
	/**
	 * Returns an instance of a .
	 * @param connection The connection used.
	 * @return An instance of a  for a specified connection.
	 */
	static function GetFactory(providerInvariantName:String):cs.system.data.common.DbProviderFactory;
	/**
	 * Returns a  that contains information about all installed providers that
	 * implement .
	 * @return A  containing  objects that contain the following data: Column ordinal
	 * Column name Description 0 **Name** Human-readable name for the data provider. 1
	 * **Description** Human-readable description of the data provider. 2
	 * **InvariantName** Name that can be used programmatically to refer to the data
	 * provider. 3 **AssemblyQualifiedName** Fully qualified name of the factory class,
	 * which contains enough information to instantiate the object.
	 */
	static function GetFactoryClasses():cs.system.data.DataTable;
	static function GetProviderInvariantNames():cs.system.collections.generic.IEnumerable<String>;
	@:overload(function(providerInvariantName:String, factory:cs.system.data.common.DbProviderFactory):Void {})
	@:overload(function(providerInvariantName:String, factoryTypeAssemblyQualifiedName:String):Void {})
	/**
	 * @param providerInvariantName 
	 * @param factory 
	 */
	static function RegisterFactory(providerInvariantName:String, providerFactoryClass:cs.system.Type):Void;
	/**
	 * @param providerInvariantName 
	 * @param factory 
	 */
	static function TryGetFactory(providerInvariantName:String, factory:cs.Ref<cs.system.data.common.DbProviderFactory>):Bool;
	/** @param providerInvariantName  */
	static function UnregisterFactory(providerInvariantName:String):Bool;
}
