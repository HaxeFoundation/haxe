[![Build Status](https://travis-ci.org/nadako/hxjsonast.svg?branch=master)](https://travis-ci.org/nadako/hxjsonast)

# hxjsonast - typed position aware JSON parsing for Haxe

This library contains a JSON parser that parses JSON (sic!) into a position-aware typed
value objects. It also contains a printer for those objects, supporting pretty-printing.

This is useful for writing all kinds of JSON validation and processing software.

The parsing and printing code comes from standard `haxe.format.JsonParser/JsonPrinter` classes,
adapted to work with custom data structures.

## Installation

```
haxelib install hxjsonast
```

## Usage

Generated API documentation is here: <https://nadako.github.io/hxjsonast/>,
but a code snippet is worth a thousand words (compile with `-lib hxjsonast`):
```haxe
import hxjsonast.*;

class Main {
    static function main() {
        var filename = 'person.json';
        var contents = '{"name": "Dan", "age": 29, "married": true}';

        // parsing is easy!
        var json = hxjsonast.Parser.parse(contents, filename);

        // `pos` store the filename, start and end characters
        trace(json.pos); // {file: 'person.json', min: 0, max: 43}

        // `value` is an enum, easy to work with pattern matching
        switch (json.value) {
            case JNull: trace('null!');
            case JString(string): trace('string!');
            case JBool(bool): trace('boolean!');
            case JNumber(number): trace('number!');
            case JArray(values): trace('array!');
            case JObject(fields): trace('object!');
        }

        // constructing Json is easy too, we just pass position and value to its constructor
        var myJson = new Json(
            JArray([
                new Json(JString("hello"), new Position("some.json", 3, 10)),
                new Json(JString("world"), new Position("other.json", 11, 30)),
            ]),
            new Position("some.json", 0, 42)
        );

        // with Haxe 3.3, we can also use new fancy @:structInit syntax instead of classic `new` operator, e.g.
        var myJson:Json = {
            pos: {file: "some.json", min: 0, max: 42},
            value: JArray([
                {
                    pos: {file: "some.json", min: 3, max: 10},
                    value: JString("hello"),
                },
                {
                    pos: {file: "other.json", min: 11, max: 30},
                    value: JString("world")
                }
            ])
        };

        // printing is easy as well (you can also pretty-print by specifying the second argument)
        var out = hxjsonast.Printer.print(myJson);
        trace(out); // ["hello","world"]

        // there's a tool to convert Json values into "normal" objects and arrays
        var value = hxjsonast.Tools.getValue(myJson);
        trace(Std.is(value, Array)); // true
    }
}
```
