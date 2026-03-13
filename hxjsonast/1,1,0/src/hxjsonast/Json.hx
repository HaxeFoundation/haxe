package hxjsonast;

/**
    This class represents a JSON value along with its position in the JSON file.
**/
@:structInit
class Json {
    /**
        Actual data of this JSON value.
    **/
    public var value:JsonValue;

    /**
        Position of this value in the JSON file.
    **/
    public var pos:Position;

    public inline function new(value:JsonValue, pos:Position) {
        this.value = value;
        this.pos = pos;
    }
}

/**
    JSON value.
**/
enum JsonValue {
    /**
        Quoted JSON string.
    **/
    JString(s:String);

    /**
        Number as defined in the JSON spec.

        Represented as `String`, because JSON spec doesn't specify minimum and maximum values,
        so it's up to the user to parse this string to a number.
    **/
    JNumber(s:String);

    /**
        JSON object.

        The fields array is ordered as defined in the JSON file.
    **/
    JObject(fields:Array<JObjectField>);

    /**
        Array of JSON values.
    **/
    JArray(values:Array<Json>);

    /**
        Boolean value.
    **/
    JBool(b:Bool);

    /**
        Null value.
    **/
    JNull;
}

/**
    JSON object field.

    See `JObject` constructor of the `JsonValue` enum.
**/
@:structInit
class JObjectField {
    /**
        Field name.
    **/
    public var name:String;

    /**
        Position of the JSON string containing field name.
    **/
    public var namePos:Position;

    /**
        Field value.
    **/
    public var value:Json;

    public inline function new(name:String, namePos:Position, value:Json) {
        this.name = name;
        this.namePos = namePos;
        this.value = value;
    }
}
