/*
 * Copyright (C)2005-2019 Haxe Foundation
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

package haxe.display;

import haxe.display.JsonModuleTypes;
import haxe.display.Position;
import haxe.display.Protocol;

@:publicFields
class ServerMethods {
	/**
		This request is sent from the client to Haxe to explore the class paths. This effectively creates a cache for toplevel completion.
	**/
	static inline var ReadClassPaths = new HaxeRequestMethod<NoData, Response<{?files:Int}>>("server/readClassPaths");

	static inline var Configure = new HaxeRequestMethod<ConfigureParams, Response<NoData>>("server/configure");
	static inline var Invalidate = new HaxeRequestMethod<FileParams, Response<NoData>>("server/invalidate");
	static inline var Contexts = new HaxeRequestMethod<NoData, Response<Array<HaxeServerContext>>>("server/contexts");
	static inline var Memory = new HaxeRequestMethod<NoData, Response<HaxeMemoryResult>>("server/memory");
	static inline var ContextMemory = new HaxeRequestMethod<ContextParams, Response<HaxeContextMemoryResult>>("server/memory/context");
	static inline var ModuleMemory = new HaxeRequestMethod<ModuleParams, Response<HaxeModuleMemoryResult>>("server/memory/module");
	static inline var Modules = new HaxeRequestMethod<ContextParams, Response<Array<String>>>("server/modules");
	static inline var Module = new HaxeRequestMethod<ModuleParams, Response<JsonModule>>("server/module");
	static inline var Type = new HaxeRequestMethod<TypeParams, Response<JsonModuleType<Any>>>("server/type");
	static inline var Files = new HaxeRequestMethod<ContextParams, Response<Array<JsonServerFile>>>("server/files");
	static inline var ModuleCreated = new HaxeRequestMethod<FileParams, Response<NoData>>("server/moduleCreated");
}

/* Configure */
typedef ConfigurePrintParams = {
	var ?addedDirectory:Bool;
	var ?foundDirectories:Bool;
	var ?changedDirectories:Bool;
	var ?modulePathChanged:Bool;
	var ?notCached:Bool;
	var ?parsed:Bool;
	var ?removedDirectory:Bool;
	var ?reusing:Bool;
	var ?skippingDep:Bool;
	var ?unchangedContent:Bool;
	var ?cachedModules:Bool;
	var ?arguments:Bool;
	var ?completion:Bool;
	var ?defines:Bool;
	var ?signature:Bool;
	var ?displayPosition:Bool;
	var ?stats:Bool;
	var ?message:Bool;
	var ?socketMessage:Bool;
	var ?uncaughtError:Bool;
	var ?newContext:Bool;
}

typedef ConfigureParams = {
	final ?noModuleChecks:Bool;
	final ?populateCacheFromDisplay:Bool;
	final ?legacyCompletion:Bool;
	final ?print:ConfigurePrintParams;
}

/* Contexts */
typedef HaxeServerContext = {
	final index:Int;
	final desc:String;
	final signature:String;
	final platform:String;
	final classPaths:Array<String>;
	final defines:Array<{key:String, value:String}>;
}

typedef ModuleId = {
	final path:String;
	final sign:String;
}

typedef JsonModule = {
	final id:Int;
	final path:JsonModulePath;
	final types:Array<JsonTypePath>;
	final file:String;
	final sign:String;
	final cacheState:Null<String>;
	final dependencies:Array<ModuleId>;
	final dependents:Array<ModuleId>;
}

typedef JsonServerFile = {
	final file:String;
	final time:Float;
	final pack:String;
	final moduleName:Null<String>;
}

typedef AdditionalSize = {
	final name:String;
	final size:Int;
	final child:Array<AdditionalSize>;
}

/* Memory */
typedef HaxeMemoryResult = {
	final contexts:Array<{
		final context:HaxeServerContext;
		final size:Int;
	}>;
	final memory:{
		final totalCache:Int;
		final contextCache:Int;
		final haxelibCache:Int;
		final directoryCache:Int;
		final nativeLibCache:Int;
		final ?additionalSizes:Array<AdditionalSize>;
	}
}

typedef HaxeContextMemoryResult = {
	final moduleCache:{
		final size:Int;
		final list:Array<{
			final path:String;
			final size:Int;
			final hasTypes:Bool;
		}>;
	};
	final syntaxCache:{
		final size:Int;
	};
	final binaryCache:{
		final size:Int;
	};
	final ?leaks:Array<{
		final path:String;
		final leaks:Array<{
			final path:String;
		}>;
	}>;
}

typedef HaxeModuleMemoryResult = {
	final moduleExtra:Int;
	final types:Array<{
		final name:String;
		final ?pos:Location;
		final size:Int;
		final fields:Array<{
			final name:String;
			final ?pos:Location;
			final size:Int;
		}>;
	}>;
}

typedef ContextParams = {
	final signature:String;
}

typedef ModuleParams = ContextParams & {
	final path:String;
}

typedef TypeParams = ContextParams & {
	final modulePath:String;
	final typeName:String;
}
