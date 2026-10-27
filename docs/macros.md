# Macros in Haxe 6: a guide for macro authors

Haxe 6 runs macros in parallel: several macros can run at the same time on
several threads, in any order. The compilation server reuses a module it typed
before, with what its macros produced, as long as its file and what it depends
on are unchanged (the modules it uses, the modules of the macros it calls and
the types they read, the files they declared); otherwise the module is typed
again and all its macros run again. The output of a compilation must stay the
same whatever the order, the number of threads, and whether it went through
the server.

Most macros need no change. The ones that do are the macros that pass
information to each other through static variables, or that depend on the
order in which types are typed. This guide explains the rules (§1), then gives
the changes to make when porting a Haxe 4 macro (§2), and how to check the
result (§3).

## 1. Principles

### 1.1 No order between macros

In Haxe 4, macros run one at a time, in the order in which the compiler types
the code. That order is an implementation detail, but many macros depend on it
without knowing: a build macro that registers its class in a static map, read
later by another macro, works only if the builds happen to run first.

In Haxe 6 there is no such order. Two expansions (two calls of a build or
expression macro) can run at the same time, and either can finish first. A
macro must give the same result whatever ran before it, and whatever runs
with it.

The result of a macro may depend on:

- its arguments, and the expression or class it is applied to;
- what it reads through the `Context` API (types, fields, metadata, defines);
- the configuration set by initialization macros (`--macro`), see §1.2;
- files it declares with `Context.registerModuleDependency`.

It must not depend on what other expansions did, except through the APIs of
§1.3.

### 1.2 Static variables

**Every expansion starts from the same state of the static variables of the
macro context**: the state their initializers and the initialization macros
(`--macro`) left. An expansion can read and change them freely, in place too
(push to an array, set a map key...), but its changes are undone when it ends.
The next expansion, on the same thread or another one, starts again from that
state.

- **Initialization macros** (`--macro`) run first, alone, before any typing.
  What they leave in static variables is the starting state of every
  expansion: this is where configuration goes.
- **A nested expansion** (a build macro run because the current macro called
  `Context.getType`, `typeof`...) also starts from that state, not from the
  state of the macro that triggered it. When it ends, the caller finds its own
  state again.
- **`@:persistent` static variables** are shared by all expansions and kept
  between the compilations of the server. They are **caches**: what is put in
  them must be a pure function of its key, so that it does not matter which
  expansion filled them, and the compiler may empty them at any time. Do not
  use them to pass information from one expansion to another.

So a static variable is the right place for:

- configuration, set by an initialization macro;
- scratch state used during one expansion (a counter, a current block, a flag);
- a cache, if `@:persistent`.

It cannot be used for registries filled by some expansions and read by others,
nor for counters shared across expansions. These use the APIs below.

### 1.3 Sharing information between macros

- **Data attached to a type**: `haxe.macro.TypeCache<T>`. A build macro gives
  the value of the class it builds (`setCurrent`); any macro reads the value of
  a type (`get`), which builds that type first if needed. A `TypeCache` can
  also compute a value from the type (a pure function), for types whose build
  gives none.
- **The whole program**: what needs every type (a list of all the classes with
  some metadata, a registry of ids) is done once typing is over, in
  `Context.onAfterTyping` or `Context.onGenerate`, where `TypeCache.all()` and
  `Context.getAllModuleTypes()` are available. Calling them during typing is an
  error, since their result would depend on what has been typed so far.
- **Types defined by macros** (`Context.defineType`): a type must be defined
  once, with the same content whichever expansion defines it.

### 1.4 Reading other types

- Getting a class (`Context.getType`, `Ref.get()`) does not build it: its
  name, package, parameters, super class, interfaces and **declared**
  metadata are available. Reading its fields builds it (runs its build
  macros).
- Metadata added by the build macros of a class is visible only after
  `Context.buildType(t)`.
- Two build macros that each need the other's class to be built form a cycle:
  this is an error, instead of one of them seeing a partially built class
  depending on the order.

### 1.5 Effects outside the compilation

- **Files**: a macro that reads a file declares it with
  `Context.registerModuleDependency(module, file)`, so that the server types
  the module again when the file changes. Writing files is the macro's
  responsibility: the compiler does not track them.
- **Other inputs** (time, environment variables, commands): call
  `Context.setVolatile()`. The server then types the module that called the
  macro again on the next compilation, instead of reusing it. With a check
  function (`Context.setVolatile(check)`), the server calls it before the next
  compilation and types the module again only if it returns `true` (or
  throws); the check runs before typing, so it can only use the system APIs
  (files, environment, time), not `Context`.
- **Console output** (`trace`, `Sys.println`) is printed immediately, in no
  guaranteed order; it is not printed when the server reuses the module, and
  can be printed twice when a macro is run again (§1.6). Use
  `Context.warning` / `Context.info` for messages that are part of the
  compilation: they are reported once.

### 1.6 A macro can be run again

To keep the threads busy, the compiler starts a macro without waiting to know
whether it will conflict with the others, and lets it run as long as it only
reads. If it turns out that the macro read something another macro changed
meanwhile (a type it looked up and another macro defined, a `@:persistent`
cache another macro filled), the run is abandoned and the macro is run again
from the start, alone this time, and sees the change.

An abandoned run has no effect on the compilation: it is stopped before it
changes anything the compiler shares (defining a type, adding metadata,
reporting a message...), and its changes to static variables are undone like
those of any expansion. What it did outside of the compiler is not undone:
console output is printed twice, a file it wrote is written twice. This is
one more reason for macros to be functions of their inputs, and to write
files in a way that does not depend on how many times they run.

## 2. Porting Haxe 4 macros

### 2.1 Configuration set by an initialization macro

Works as it is: what an initialization macro writes to a static variable,
replaced or changed in place, is the starting state of every expansion.

```haxe
class Config {
	static var prefix = "default";
	static var ignored = new Map<String, Bool>();

	// --macro Config.setup("my")
	public static function setup(p:String) {
		prefix = p;
		ignored.set("debug", true);
	}

	public static function build() {
		// sees prefix == "my" and ignored["debug"], in every expansion
		return null;
	}
}
```

Configuration given by something else than an initialization macro is not
kept: for instance a build macro on an interface that sets options for the
builds of its implementers. Move it to an initialization macro, or make the
variable `@:persistent` if writing it twice gives the same value.

### 2.2 Registry of built classes

Haxe 4: each build registers its class in a static map, which other macros
read.

```haxe
static var components = new Map<String, ComponentInfo>();

public static function build() {
	var c = Context.getLocalClass().get();
	components.set(c.name, makeInfo(c));
	return null;
}

public static function lookup(name:String) {
	return components.get(name); // null if not built yet: depends on the order
}
```

Haxe 6: the build gives the value of its class to a `TypeCache`; a reader asks
for the value of a type, which builds it if needed.

```haxe
static var components = new haxe.macro.TypeCache<ComponentInfo>();

public static function build() {
	var c = Context.getLocalClass().get();
	components.setCurrent(makeInfo(c));
	return null;
}

public static function lookup(path:String) {
	return components.getByPath(path); // builds the class if needed
}
```

`tryGet(t)` returns `null` for a type whose build gave no value, instead of
failing. The value must not be changed once given.

### 2.3 Data computed from a type

Haxe 4: a static cache filled on first use.

```haxe
static var parsed = new Map<String, ShaderData>();

static function getShader(t:Type) {
	var key = t.toString();
	var s = parsed.get(key);
	if (s == null) {
		s = parse(t);
		parsed.set(key, s);
	}
	return s;
}
```

Haxe 6: a `TypeCache` with a compute function, which must be a pure function
of the type. The compiler computes the value once and knows what it depends
on.

```haxe
static var parsed = new haxe.macro.TypeCache<ShaderData>(t -> parse(t));

static function getShader(t:Type) {
	return parsed.get(t);
}
```

A plain cache keyed by something else than a type (a parsed file, a converted
resource) can stay a map, made `@:persistent`, as long as its values are pure
functions of their keys:

```haxe
@:persistent static var parsedFiles = new Map<String, Data>();
```

### 2.4 Whole-program processing

Haxe 4: a build macro collects classes, and an `onGenerate` (or the last
macro to run) uses the list.

```haxe
static var all:Array<String> = [];

public static function build() {
	all.push(Context.getLocalClass().toString());
	return null;
}

public static function init() {
	Context.onGenerate(_ -> generateRegistry(all));
}
```

Haxe 6: the builds give their value to a `TypeCache`, and the list is read once
typing is done. The callback can define a type with the result.

```haxe
using haxe.macro.TypeTools;

static var entries = new haxe.macro.TypeCache<Bool>();

public static function build() {
	entries.setCurrent(true);
	return null;
}

// --macro Registry.init()
public static function init() {
	Context.onAfterTyping(types -> {
		// called again once the types it defined are typed
		for (t in types)
			switch t {
				case TClassDecl(_.get() => c) if (c.name == "AllEntries"): return;
				default:
			}
		var names = [for (e in entries.all()) e.type.toString()]; // sorted by path
		Context.defineType(makeRegistryClass("AllEntries", names));
	});
}
```

Like every callback, `onAfterTyping` starts from the initial state of the
static variables: a Haxe 4 `static var done = false` guard does not stop the
second call, check the types instead. The code typed before the callback ran
cannot name the class it defines: reach it by reflection
(`Type.resolveClass`), and give it `@:keep`.

`Context.getAllModuleTypes()` follows the same rule: call it from
`onAfterTyping` or `onGenerate`, not during typing.

### 2.5 Unique names and counters

Haxe 4: a static counter gives unique names.

```haxe
static var counter = 0;

public static macro function makeTemp() {
	var name = "__tmp" + counter++;
	...
}
```

In Haxe 6 the counter starts from 0 in every expansion. Derive the name from
the context of the expansion instead: the class being built, the field, the
position, or the fields already there.

```haxe
public static macro function makeTemp() {
	var pos = Context.getPosInfos(Context.currentPos());
	var name = "__tmp_" + pos.min;
	...
}

// in a build macro: count what this build already added
var id = "_" + [for (f in fields) if (StringTools.startsWith(f.name, "_")) f].length;
```

A counter used only during one expansion (to number the locals the macro
generates, for instance) works as before.

### 2.6 Helper types defined on demand

The usual pattern stays valid:

```haxe
static function getProxy(name:String, fields:Array<Field>) {
	var path = "proxies." + name;
	try {
		return Context.getType(path);
	} catch (e:Dynamic) {
		Context.defineType(makeProxy(name, fields));
		return Context.getType(path);
	}
}
```

Two expansions running at the same time can both fail to find the type. The
first one to define it defines it; the other one is then run again (§1.6)
before it defines anything, and finds the type. The type is defined once,
by whichever expansion comes first: **its definition must be the same
whichever expansion makes it**. Build its content from `name` and from what
the macro reads through the API, not from the state of the expansion that
defines it. Defining a type without checking first that it exists is an
error the second time ("Cannot redefine module").

### 2.7 Reading a class built by other macros

Haxe 4: reading a class (`Ref.get()`, `getFields`) often triggered its build,
or saw it half built, depending on the order.

Haxe 6: reading a field builds the class; its metadata is the declared one
until it is built. To read metadata or fields added by its build macros, build
it first:

```haxe
var t = Context.getType("ui.Button");
Context.buildType(t);
var meta = switch t { case TInst(c, _): c.get().meta.get(); default: []; };
```

Two classes whose builds need each other built are a cycle error: split the
information so that one of the builds only needs the declared part of the
other (its declared metadata, its super class), or give it through a
`TypeCache` computed from the declaration.

### 2.8 `onTypeNotFound`

Haxe 6 accepts `Context.onTypeNotFound` only in an initialization macro: a type
generated on demand must not depend on which expansion asked for it first.
Register the callback from `--macro`.

### 2.9 Files, environment, time

```haxe
public static macro function embed(file:String) {
	var path = Context.resolvePath(file);
	Context.registerModuleDependency(Context.getLocalModule(), path);
	return macro $v{sys.io.File.getContent(path)};
}

public static macro function buildDate() {
	Context.setVolatile(); // the module is typed again on every compilation
	return macro $v{Date.now().toString()};
}
```

With a check, the server types the module again only when the check returns
`true`:

```haxe
var dir = Sys.getEnv("ASSETS");
Context.setVolatile(() -> Sys.getEnv("ASSETS") != dir);
```

A macro writing files must write the same files whatever the order of the
expansions: write one file per expansion (named after its class), or write
everything from `onAfterTyping` / `onAfterGenerate`.

## 3. Checking a port

The output of a compilation must not change with the order of typing nor the
number of threads. Compile the project several ways and compare the generated
files:

- `-D parallel-typing=0 -D typer-shuffle=<seed>` with a few seeds: one thread,
  a random order of typing;
- `-D parallel-typing=<n>` with a few numbers of threads (1, 2, 8...);
- through the compilation server (`--wait` / `--connect`), after editing a
  file, against a compilation from scratch.

A macro that still depends on the order usually shows up as a different output
between seeds. `-D macro-state-check` also checks that the values given to a
`TypeCache` are not changed after they are given, and that its compute
function gives the same value when called again.
