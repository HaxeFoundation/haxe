package json2object.utils.schema;

typedef JsonSchemaType = {
	@:optional
	var const: Null<String>;
	@:optional @:alias('const')
	var const_bool : Bool;
	@:optional @:alias('const')
	var const_int : Int;
	@:optional @:alias('const')
	var const_float : Float;
	@:optional @:alias('default') @:noquoting
	var defaultValue : String;
	@:optional
	var description: String;
	@:optional
	var markdownDescription: String;
	@:optional
	var type: String;
	@:optional @:alias("$schema")
	var schema: String;
	@:optional @:alias("$ref")
	var ref: String;
	@:optional
	var required: Array<String>;
	@:optional
	var properties: Map<String, JsonSchemaType>;
	@:optional
	var additionalProperties: Bool;
	@:optional @:alias("additionalProperties")
	var additionalProperties_obj: JsonSchemaType;
	@:optional
	var items: JsonSchemaType;
	@:optional
	var patternProperties: Map<String, JsonSchemaType>;
	@:optional
	var anyOf: Array<JsonSchemaType>;
	@:optional
	var definitions: Map<String, JsonSchemaType>;
	@:optional
	var markdownEnumDescriptions: Array<String>;
	@:optional
	var enumDescriptions: Array<String>;
	@:optional @:alias("enum")
	var enum_string: Array<Null<String>>;
	@:optional @:alias("enum")
	var enum_int: Array<Null<Int>>;
	@:optional @:alias("enum")
	var enum_float: Array<Null<Float>>;
	@:optional @:alias("enum")
	var enum_bool: Array<Null<Bool>>;
}
