package cs.system.data;

/** Represents an in-memory cache of data. */
@:native("System.Data.DataSet")
extern class DataSet extends cs.system.componentmodel.MarshalByValueComponent {
	/**
	 * Gets or sets a value indicating whether string comparisons within  objects are
	 * case-sensitive.
	 * @return if string comparisons are case-sensitive; otherwise, . The default is .
	 */
	var CaseSensitive(default, default):Bool;
	/**
	 * Gets or sets the name of the current .
	 * @return The name of the .
	 */
	var DataSetName(default, default):String;
	/**
	 * Gets a custom view of the data contained in the  to allow filtering, searching,
	 * and navigating using a custom .
	 * @return A  object.
	 */
	var DefaultViewManager(default, never):cs.system.data.DataViewManager;
	/**
	 * Gets or sets a value indicating whether constraint rules are followed when
	 * attempting any update operation.
	 * @return if rules are enforced; otherwise, . The default is .
	 */
	var EnforceConstraints(default, default):Bool;
	/**
	 * Gets the collection of customized user information associated with the .
	 * @return A  with all custom user information.
	 */
	var ExtendedProperties(default, never):cs.system.data.PropertyCollection;
	/**
	 * Gets a value indicating whether there are errors in any of the  objects within
	 * this .
	 * @return if any table contains an error; otherwise, .
	 */
	var HasErrors(default, never):Bool;
	/**
	 * Gets a value that indicates whether the  is initialized.
	 * @return to indicate the component has completed initialization; otherwise, .
	 */
	var IsInitialized(default, never):Bool;
	/**
	 * Gets or sets the locale information used to compare strings within the table.
	 * @return A  that contains data about the user's machine locale. The default is .
	 */
	var Locale(default, default):cs.system.globalization.CultureInfo;
	/**
	 * Gets or sets the namespace of the .
	 * @return The namespace of the .
	 */
	var Namespace(default, default):String;
	/**
	 * Gets or sets an XML prefix that aliases the namespace of the .
	 * @return The XML prefix for the  namespace.
	 */
	var Prefix(default, default):String;
	/**
	 * Gets the collection of relations that link tables and allow navigation from
	 * parent tables to child tables.
	 * @return A  that contains a collection of  objects. An empty collection is
	 * returned if no  objects exist.
	 */
	var Relations(default, never):cs.system.data.DataRelationCollection;
	/**
	 * Gets or sets a  for the  used during remoting.
	 * @return A  object.
	 */
	var RemotingFormat(default, default):cs.system.data.SerializationFormat;
	/**
	 * Gets or sets a  for a .
	 * @return A  for a .
	 */
	var SchemaSerializationMode(default, default):cs.system.data.SchemaSerializationMode;
	/**
	 * Gets the collection of tables contained in the .
	 * @return The  contained by this . An empty collection is returned if no  objects
	 * exist.
	 */
	var Tables(default, never):cs.system.data.DataTableCollection;
	@:overload(function():Void {})
	function new(dataSetName:String):Void;
	/**
	 * Gets a copy of  for the DataSet.
	 * @param schemaSet The specified schema set.
	 * @return A copy of .
	 */
	static function GetDataSetSchema(schemaSet:cs.system.xml.schema.XmlSchemaSet):cs.system.xml.schema.XmlSchemaComplexType;
	/** Commits all the changes made to this  since it was loaded or since the last time  was called. */
	function AcceptChanges():Void;
	/** Begins the initialization of a  that is used on a form or used by another component. The initialization occurs at run time. */
	function BeginInit():Void;
	/** Clears the  of any data by removing all rows in all tables. */
	function Clear():Void;
	/**
	 * Copies the structure of the , including all  schemas, relations, and
	 * constraints. Does not copy any data.
	 * @return A new  with the same schema as the current , but none of the data.
	 */
	function Clone():cs.system.data.DataSet;
	/**
	 * Copies both the structure and data for this .
	 * @return A new  with the same structure (table schemas, relations, and
	 * constraints) and data as this . If these classes have been subclassed, the copy
	 * will also be of the same subclasses.
	 */
	function Copy():cs.system.data.DataSet;
	@:overload(function():cs.system.data.DataTableReader {})
	/**
	 * Returns a  with one result set per , in the same sequence as the tables appear
	 * in the  collection.
	 * @return A  containing one or more result sets, corresponding to the  instances
	 * contained within the source .
	 */
	function CreateDataReader(dataTables:cs.NativeArray<cs.system.data.DataTable>):cs.system.data.DataTableReader;
	/** Ends the initialization of a  that is used on a form or used by another component. The initialization occurs at run time. */
	function EndInit():Void;
	@:overload(function():cs.system.data.DataSet {})
	/**
	 * Gets a copy of the  that contains all changes made to it since it was loaded or
	 * since  was last called.
	 * @return A copy of the changes from this  that can have actions performed on it
	 * and later be merged back in using . If no changed rows are found, the method
	 * returns .
	 */
	function GetChanges(rowStates:cs.system.data.DataRowState):cs.system.data.DataSet;
	/**
	 * Populates a serialization information object with the data needed to serialize
	 * the .
	 * @param info A  that holds the serialized data associated with the .
	 * @param context A  that contains the source and destination of the serialized
	 * stream associated with the .
	 */
	function GetObjectData(info:cs.system.runtime.serialization.SerializationInfo, context:cs.system.runtime.serialization.StreamingContext):Void;
	/**
	 * Returns the XML representation of the data stored in the .
	 * @return A string that is a representation of the data stored in the .
	 */
	function GetXml():String;
	/**
	 * Returns the XML Schema for the XML representation of the data stored in the .
	 * @return String that is the XML Schema for the XML representation of the data
	 * stored in the .
	 */
	function GetXmlSchema():String;
	@:overload(function():Bool {})
	/**
	 * Gets a value indicating whether the  has changes, including new, deleted, or
	 * modified rows.
	 * @return if the  has changes; otherwise, .
	 */
	function HasChanges(rowStates:cs.system.data.DataRowState):Bool;
	@:overload(function(stream:cs.system.io.Stream, nsArray:cs.NativeArray<String>):Void {})
	@:overload(function(reader:cs.system.io.TextReader, nsArray:cs.NativeArray<String>):Void {})
	@:overload(function(fileName:String, nsArray:cs.NativeArray<String>):Void {})
	/**
	 * Applies the XML schema from the specified  to the .
	 * @param stream The  from which to read the schema.
	 * @param nsArray An array of namespace Uniform Resource Identifier (URI) strings
	 * to be excluded from schema inference.
	 */
	function InferXmlSchema(reader:cs.system.xml.XmlReader, nsArray:cs.NativeArray<String>):Void;
	@:overload(function(reader:cs.system.data.IDataReader, loadOption:cs.system.data.LoadOption, tables:cs.NativeArray<cs.system.data.DataTable>):Void {})
	@:overload(function(reader:cs.system.data.IDataReader, loadOption:cs.system.data.LoadOption, tables:cs.NativeArray<String>):Void {})
	/**
	 * Fills a  with values from a data source using the supplied , using an array of 
	 * instances to supply the schema and namespace information.
	 * @param reader An  that provides one or more result sets.
	 * @param loadOption A value from the  enumeration that indicates how rows already
	 * in the  instances within the  will be combined with incoming rows that share the
	 * same primary key.
	 * @param tables An array of  instances, from which the  method retrieves name and
	 * namespace information. Each of these tables must be a member of the  contained
	 * by this .
	 */
	function Load(reader:cs.system.data.IDataReader, loadOption:cs.system.data.LoadOption, errorHandler:cs.system.data.FillErrorEventHandler, tables:cs.NativeArray<cs.system.data.DataTable>):Void;
	@:overload(function(rows:cs.NativeArray<cs.system.data.DataRow>):Void {})
	@:overload(function(dataSet:cs.system.data.DataSet):Void {})
	@:overload(function(table:cs.system.data.DataTable):Void {})
	@:overload(function(dataSet:cs.system.data.DataSet, preserveChanges:Bool):Void {})
	@:overload(function(rows:cs.NativeArray<cs.system.data.DataRow>, preserveChanges:Bool, missingSchemaAction:cs.system.data.MissingSchemaAction):Void {})
	@:overload(function(dataSet:cs.system.data.DataSet, preserveChanges:Bool, missingSchemaAction:cs.system.data.MissingSchemaAction):Void {})
	/**
	 * Merges an array of  objects into the current .
	 * @param rows The array of  objects to be merged into the .
	 */
	function Merge(table:cs.system.data.DataTable, preserveChanges:Bool, missingSchemaAction:cs.system.data.MissingSchemaAction):Void;
	@:overload(function(stream:cs.system.io.Stream):cs.system.data.XmlReadMode {})
	@:overload(function(reader:cs.system.io.TextReader):cs.system.data.XmlReadMode {})
	@:overload(function(fileName:String):cs.system.data.XmlReadMode {})
	@:overload(function(reader:cs.system.xml.XmlReader):cs.system.data.XmlReadMode {})
	@:overload(function(stream:cs.system.io.Stream, mode:cs.system.data.XmlReadMode):cs.system.data.XmlReadMode {})
	@:overload(function(reader:cs.system.io.TextReader, mode:cs.system.data.XmlReadMode):cs.system.data.XmlReadMode {})
	@:overload(function(fileName:String, mode:cs.system.data.XmlReadMode):cs.system.data.XmlReadMode {})
	/**
	 * Reads XML schema and data into the  using the specified .
	 * @param stream An object that derives from .
	 * @return The  used to read the data.
	 */
	function ReadXml(reader:cs.system.xml.XmlReader, mode:cs.system.data.XmlReadMode):cs.system.data.XmlReadMode;
	@:overload(function(stream:cs.system.io.Stream):Void {})
	@:overload(function(reader:cs.system.io.TextReader):Void {})
	@:overload(function(fileName:String):Void {})
	/**
	 * Reads the XML schema from the specified  into the .
	 * @param stream The  from which to read.
	 */
	function ReadXmlSchema(reader:cs.system.xml.XmlReader):Void;
	/** Rolls back all the changes made to the  since it was created, or since the last time  was called. */
	function RejectChanges():Void;
	/** Clears all tables and removes all relations, foreign constraints, and tables from the . Subclasses should override  to restore a  to its original state. */
	function Reset():Void;
	@:overload(function(stream:cs.system.io.Stream):Void {})
	@:overload(function(writer:cs.system.io.TextWriter):Void {})
	@:overload(function(fileName:String):Void {})
	@:overload(function(writer:cs.system.xml.XmlWriter):Void {})
	@:overload(function(stream:cs.system.io.Stream, mode:cs.system.data.XmlWriteMode):Void {})
	@:overload(function(writer:cs.system.io.TextWriter, mode:cs.system.data.XmlWriteMode):Void {})
	@:overload(function(fileName:String, mode:cs.system.data.XmlWriteMode):Void {})
	/**
	 * Writes the current data for the  using the specified .
	 * @param stream A  object used to write to a file.
	 */
	function WriteXml(writer:cs.system.xml.XmlWriter, mode:cs.system.data.XmlWriteMode):Void;
	@:overload(function(stream:cs.system.io.Stream):Void {})
	@:overload(function(writer:cs.system.io.TextWriter):Void {})
	@:overload(function(fileName:String):Void {})
	@:overload(function(writer:cs.system.xml.XmlWriter):Void {})
	@:overload(function(stream:cs.system.io.Stream, multipleTargetConverter:cs.system.Converter<cs.system.Type, String>):Void {})
	@:overload(function(writer:cs.system.io.TextWriter, multipleTargetConverter:cs.system.Converter<cs.system.Type, String>):Void {})
	@:overload(function(fileName:String, multipleTargetConverter:cs.system.Converter<cs.system.Type, String>):Void {})
	/**
	 * Writes the  structure as an XML schema to the specified  object.
	 * @param stream A  object used to write to a file.
	 */
	function WriteXmlSchema(writer:cs.system.xml.XmlWriter, multipleTargetConverter:cs.system.Converter<cs.system.Type, String>):Void;
}
