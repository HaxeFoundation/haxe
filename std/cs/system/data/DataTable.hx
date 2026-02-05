package cs.system.data;

/** Represents one table of in-memory data. */
@:native("System.Data.DataTable")
extern class DataTable extends cs.system.componentmodel.MarshalByValueComponent {
	/**
	 * Indicates whether string comparisons within the table are case-sensitive.
	 * @return if the comparison is case-sensitive; otherwise . The default is set to
	 * the parent  object's  property, or  if the  was created independently of a .
	 */
	var CaseSensitive(default, default):Bool;
	/**
	 * Gets the collection of child relations for this .
	 * @return A  that contains the child relations for the table. An empty collection
	 * is returned if no  objects exist.
	 */
	var ChildRelations(default, never):cs.system.data.DataRelationCollection;
	/**
	 * Gets the collection of columns that belong to this table.
	 * @return A  that contains the collection of  objects for the table. An empty
	 * collection is returned if no  objects exist.
	 */
	var Columns(default, never):cs.system.data.DataColumnCollection;
	/**
	 * Gets the collection of constraints maintained by this table.
	 * @return A  that contains the collection of  objects for the table. An empty
	 * collection is returned if no  objects exist.
	 */
	var Constraints(default, never):cs.system.data.ConstraintCollection;
	/**
	 * Gets the  to which this table belongs.
	 * @return The  to which this table belongs.
	 */
	var DataSet(default, never):cs.system.data.DataSet;
	/**
	 * Gets a customized view of the table that may include a filtered view, or a
	 * cursor position.
	 * @return The  associated with the .
	 */
	var DefaultView(default, never):cs.system.data.DataView;
	/**
	 * Gets or sets the expression that returns a value used to represent this table in
	 * the user interface. The  property lets you display the name of this table in a
	 * user interface.
	 * @return A display string.
	 */
	var DisplayExpression(default, default):String;
	/**
	 * Gets the collection of customized user information.
	 * @return A  that contains custom user information.
	 */
	var ExtendedProperties(default, never):cs.system.data.PropertyCollection;
	/**
	 * Gets a value indicating whether there are errors in any of the rows in any of
	 * the tables of the  to which the table belongs.
	 * @return if errors exist; otherwise .
	 */
	var HasErrors(default, never):Bool;
	/**
	 * Gets a value that indicates whether the  is initialized.
	 * @return to indicate the component has completed initialization; otherwise .
	 */
	var IsInitialized(default, never):Bool;
	/**
	 * Gets or sets the locale information used to compare strings within the table.
	 * @return A  that contains data about the user's machine locale. The default is
	 * the  object's  (returned by the  property) to which the  belongs; if the table
	 * doesn't belong to a , the default is the current system .
	 */
	var Locale(default, default):cs.system.globalization.CultureInfo;
	/**
	 * Gets or sets the initial starting size for this table.
	 * @return The initial starting size in rows of this table. The default is 50.
	 */
	var MinimumCapacity(default, default):Int;
	/**
	 * Gets or sets the namespace for the XML representation of the data stored in the
	 * .
	 * @return The namespace of the .
	 */
	var Namespace(default, default):String;
	/**
	 * Gets the collection of parent relations for this .
	 * @return A  that contains the parent relations for the table. An empty collection
	 * is returned if no  objects exist.
	 */
	var ParentRelations(default, never):cs.system.data.DataRelationCollection;
	/**
	 * Gets or sets the namespace for the XML representation of the data stored in the
	 * .
	 * @return The prefix of the .
	 */
	var Prefix(default, default):String;
	/**
	 * Gets or sets an array of columns that function as primary keys for the data
	 * table.
	 * @return An array of  objects.
	 */
	var PrimaryKey(default, default):cs.NativeArray<cs.system.data.DataColumn>;
	/**
	 * Gets or sets the serialization format.
	 * @return A  enumeration specifying either  or  serialization.
	 */
	var RemotingFormat(default, default):cs.system.data.SerializationFormat;
	/**
	 * Gets the collection of rows that belong to this table.
	 * @return A  that contains  objects; otherwise a null value if no  objects exist.
	 */
	var Rows(default, never):cs.system.data.DataRowCollection;
	/**
	 * Gets or sets the name of the .
	 * @return The name of the .
	 */
	var TableName(default, default):String;
	@:overload(function():Void {})
	@:overload(function(tableName:String):Void {})
	function new(tableName:String, tableNamespace:String):Void;
	/**
	 * This method returns an  instance containing the Web Services Description
	 * Language (WSDL) that describes the  for Web Services.
	 * @param schemaSet An  instance.
	 * @return The  instance.
	 */
	static function GetDataTableSchema(schemaSet:cs.system.xml.schema.XmlSchemaSet):cs.system.xml.schema.XmlSchemaComplexType;
	/** Commits all the changes made to this table since the last time  was called. */
	function AcceptChanges():Void;
	/** Begins the initialization of a  that is used on a form or used by another component. The initialization occurs at run time. */
	function BeginInit():Void;
	/** Turns off notifications, index maintenance, and constraints while loading data. */
	function BeginLoadData():Void;
	/** Clears the  of all data. */
	function Clear():Void;
	/**
	 * Clones the structure of the , including all  schemas and constraints.
	 * @return A new  with the same schema as the current .
	 */
	function Clone():cs.system.data.DataTable;
	/**
	 * Computes the given expression on the current rows that pass the filter criteria.
	 * @param expression The expression to compute.
	 * @param filter The filter to limit the rows that evaluate in the expression.
	 * @return An , set to the result of the computation. If the expression evaluates
	 * to null, the return value will be .
	 */
	function Compute(expression:String, filter:String):Dynamic;
	/**
	 * Copies both the structure and data for this .
	 * @return A new  with the same structure (table schemas and constraints) and data
	 * as this . If these classes have been derived, the copy will also be of the same
	 * derived classes. creates a new  with the same structure and data as the original
	 * . To copy the structure to a new , but not the data, use .
	 */
	function Copy():cs.system.data.DataTable;
	/**
	 * Returns a  corresponding to the data within this .
	 * @return A  containing one result set, corresponding to the source  instance.
	 */
	function CreateDataReader():cs.system.data.DataTableReader;
	/** Ends the initialization of a  that is used on a form or used by another component. The initialization occurs at run time. */
	function EndInit():Void;
	/** Turns on notifications, index maintenance, and constraints after loading data. */
	function EndLoadData():Void;
	@:overload(function():cs.system.data.DataTable {})
	/**
	 * Gets a copy of the  that contains all changes made to it since it was loaded or 
	 * was last called.
	 * @return A copy of the changes from this , or  if no changes are found.
	 */
	function GetChanges(rowStates:cs.system.data.DataRowState):cs.system.data.DataTable;
	/**
	 * Gets an array of  objects that contain errors.
	 * @return An array of  objects that have errors.
	 */
	function GetErrors():cs.NativeArray<cs.system.data.DataRow>;
	/**
	 * Populates a serialization information object with the data needed to serialize
	 * the .
	 * @param info A  object that holds the serialized data associated with the .
	 * @param context A  object that contains the source and destination of the
	 * serialized stream associated with the .
	 */
	function GetObjectData(info:cs.system.runtime.serialization.SerializationInfo, context:cs.system.runtime.serialization.StreamingContext):Void;
	/**
	 * Copies a  into a , preserving any property settings, as well as original and
	 * current values.
	 * @param row The  to be imported.
	 */
	function ImportRow(row:cs.system.data.DataRow):Void;
	@:overload(function(reader:cs.system.data.IDataReader):Void {})
	@:overload(function(reader:cs.system.data.IDataReader, loadOption:cs.system.data.LoadOption):Void {})
	/**
	 * Fills a  with values from a data source using the supplied . If the  already
	 * contains rows, the incoming data from the data source is merged with the
	 * existing rows.
	 * @param reader An  that provides a result set.
	 */
	function Load(reader:cs.system.data.IDataReader, loadOption:cs.system.data.LoadOption, errorHandler:cs.system.data.FillErrorEventHandler):Void;
	@:overload(function(values:cs.NativeArray<Dynamic>, fAcceptChanges:Bool):cs.system.data.DataRow {})
	/**
	 * Finds and updates a specific row. If no matching row is found, a new row is
	 * created using the given values.
	 * @param values An array of values used to create the new row.
	 * @param fAcceptChanges to accept changes; otherwise .
	 * @return The new .
	 */
	function LoadDataRow(values:cs.NativeArray<Dynamic>, loadOption:cs.system.data.LoadOption):cs.system.data.DataRow;
	@:overload(function(table:cs.system.data.DataTable):Void {})
	@:overload(function(table:cs.system.data.DataTable, preserveChanges:Bool):Void {})
	/**
	 * Merge the specified  with the current .
	 * @param table The  to be merged with the current .
	 */
	function Merge(table:cs.system.data.DataTable, preserveChanges:Bool, missingSchemaAction:cs.system.data.MissingSchemaAction):Void;
	/**
	 * Creates a new  with the same schema as the table.
	 * @return A  with the same schema as the .
	 */
	function NewRow():cs.system.data.DataRow;
	@:overload(function(stream:cs.system.io.Stream):cs.system.data.XmlReadMode {})
	@:overload(function(reader:cs.system.io.TextReader):cs.system.data.XmlReadMode {})
	@:overload(function(fileName:String):cs.system.data.XmlReadMode {})
	/**
	 * Reads XML schema and data into the  using the specified .
	 * @param stream An object that derives from
	 * @return The  used to read the data.
	 */
	function ReadXml(reader:cs.system.xml.XmlReader):cs.system.data.XmlReadMode;
	@:overload(function(stream:cs.system.io.Stream):Void {})
	@:overload(function(reader:cs.system.io.TextReader):Void {})
	@:overload(function(fileName:String):Void {})
	/**
	 * Reads an XML schema into the  using the specified stream.
	 * @param stream The stream used to read the schema.
	 */
	function ReadXmlSchema(reader:cs.system.xml.XmlReader):Void;
	/** Rolls back all changes that have been made to the table since it was loaded, or the last time  was called. */
	function RejectChanges():Void;
	/** Resets the  to its original state. Reset removes all data, indexes, relations, and columns of the table. If a DataSet includes a DataTable, the table will still be part of the DataSet after the table is reset. */
	function Reset():Void;
	@:overload(function():cs.NativeArray<cs.system.data.DataRow> {})
	@:overload(function(filterExpression:String):cs.NativeArray<cs.system.data.DataRow> {})
	@:overload(function(filterExpression:String, sort:String):cs.NativeArray<cs.system.data.DataRow> {})
	/**
	 * Gets an array of all  objects.
	 * @return An array of  objects.
	 */
	function Select(filterExpression:String, sort:String, recordStates:cs.system.data.DataViewRowState):cs.NativeArray<cs.system.data.DataRow>;
	/**
	 * Gets the  and , if there is one as a concatenated string.
	 * @return A string consisting of the  and the  values.
	 */
	function ToString():String;
	@:overload(function(stream:cs.system.io.Stream):Void {})
	@:overload(function(writer:cs.system.io.TextWriter):Void {})
	@:overload(function(fileName:String):Void {})
	@:overload(function(writer:cs.system.xml.XmlWriter):Void {})
	@:overload(function(stream:cs.system.io.Stream, writeHierarchy:Bool):Void {})
	@:overload(function(stream:cs.system.io.Stream, mode:cs.system.data.XmlWriteMode):Void {})
	@:overload(function(writer:cs.system.io.TextWriter, writeHierarchy:Bool):Void {})
	@:overload(function(writer:cs.system.io.TextWriter, mode:cs.system.data.XmlWriteMode):Void {})
	@:overload(function(fileName:String, writeHierarchy:Bool):Void {})
	@:overload(function(fileName:String, mode:cs.system.data.XmlWriteMode):Void {})
	@:overload(function(writer:cs.system.xml.XmlWriter, writeHierarchy:Bool):Void {})
	@:overload(function(writer:cs.system.xml.XmlWriter, mode:cs.system.data.XmlWriteMode):Void {})
	@:overload(function(stream:cs.system.io.Stream, mode:cs.system.data.XmlWriteMode, writeHierarchy:Bool):Void {})
	@:overload(function(writer:cs.system.io.TextWriter, mode:cs.system.data.XmlWriteMode, writeHierarchy:Bool):Void {})
	@:overload(function(fileName:String, mode:cs.system.data.XmlWriteMode, writeHierarchy:Bool):Void {})
	/**
	 * Writes the current contents of the  as XML using the specified .
	 * @param stream The stream to which the data will be written.
	 */
	function WriteXml(writer:cs.system.xml.XmlWriter, mode:cs.system.data.XmlWriteMode, writeHierarchy:Bool):Void;
	@:overload(function(stream:cs.system.io.Stream):Void {})
	@:overload(function(writer:cs.system.io.TextWriter):Void {})
	@:overload(function(fileName:String):Void {})
	@:overload(function(writer:cs.system.xml.XmlWriter):Void {})
	@:overload(function(stream:cs.system.io.Stream, writeHierarchy:Bool):Void {})
	@:overload(function(writer:cs.system.io.TextWriter, writeHierarchy:Bool):Void {})
	@:overload(function(fileName:String, writeHierarchy:Bool):Void {})
	/**
	 * Writes the current data structure of the  as an XML schema to the specified
	 * stream.
	 * @param stream The stream to which the XML schema will be written.
	 */
	function WriteXmlSchema(writer:cs.system.xml.XmlWriter, writeHierarchy:Bool):Void;
}
