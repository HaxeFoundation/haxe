/*
 * Copyright (C)2005-2018 Haxe Foundation
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 */
package haxe.rtti;

import haxe.rtti.JsonModuleTypes;

class JsonModuleTypesPrinter {
    public function new() {

    }

    public function printPath(path:JsonPath) {
        if (path.pack.length == 0) {
            return path.name;
        } else {
            return path.pack.join(".") + "." + path.name;
        }
    }

    public function printPathWithParams(path:JsonPathWithParams) {
        var s = printPath(path.path);
        if (path.params.length == 0) {
            return s;
        } else {
            var sparams = path.params.map(printType).join(", ");
            return '$s<$sparams>';
        }
    }

    public function printType<T>(t:JsonType<T>) {
        return switch (t.kind) {
            case TMono: "?";
            case TInst | TEnum | TType | TAbstract: printPathWithParams(t.args);
            case TDynamic:
                if (t.args == null) {
                    "Dynamic";
                } else {
                    var s = printType(t.args);
                    'Dynamic<$s>';
                }
            case TAnonymous:
                var fields = t.args.fields;
                var s = [for (field in fields) '${field.name}: ${printType(field.type)}'].join(", ");
                '{ $s }';
            case TFun:
                function printFunctionArgument(arg:JsonFunctionArgument) {
                    return (arg.opt ? "?" : "") + arg.name + ":" + printType(arg.t);
                }
                var s = t.args.args.map(printFunctionArgument).join(", ");
                '($s) -> ${printType(t.args.ret)}';
        }
    }
}