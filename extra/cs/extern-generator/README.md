# C# Extern Generator

Generates Haxe extern definitions from .NET assemblies.

## Overview

This tool reads .NET DLL files and their accompanying XML documentation to produce Haxe extern type definitions that can be used with the C# target.

## Requirements

- .NET 6.0 or later

## Building

```bash
cd extra/cs/extern-generator
dotnet build
```

## Creating Standalone Executables

You can create standalone executables that don't require .NET to be installed on the target machine.

### Native AOT (Recommended - smaller size ~23MB)

Native AOT compiles to native machine code with no runtime dependency:

```bash
# Mac (Apple Silicon)
dotnet publish -c Release -r osx-arm64 -p:PublishAot=true -o ./publish-aot

# Mac (Intel)
dotnet publish -c Release -r osx-x64 -p:PublishAot=true -o ./publish-aot

# Linux (x64)
dotnet publish -c Release -r linux-x64 -p:PublishAot=true -o ./publish-aot

# Linux (ARM64)
dotnet publish -c Release -r linux-arm64 -p:PublishAot=true -o ./publish-aot

# Windows (x64)
dotnet publish -c Release -r win-x64 -p:PublishAot=true -o ./publish-aot
```

### Self-Contained Single File (larger size ~74MB)

This bundles the .NET runtime into a single executable:

```bash
# Mac (Apple Silicon)
dotnet publish -c Release -r osx-arm64 --self-contained true -p:PublishSingleFile=true -o ./publish

# Mac (Intel)
dotnet publish -c Release -r osx-x64 --self-contained true -p:PublishSingleFile=true -o ./publish

# Linux (x64)
dotnet publish -c Release -r linux-x64 --self-contained true -p:PublishSingleFile=true -o ./publish

# Linux (ARM64)
dotnet publish -c Release -r linux-arm64 --self-contained true -p:PublishSingleFile=true -o ./publish

# Windows (x64)
dotnet publish -c Release -r win-x64 --self-contained true -p:PublishSingleFile=true -o ./publish
```

The resulting executable will be in the specified output directory (`./publish-aot` or `./publish`).

## Usage

### Generate externs from a DLL

```bash
dotnet run -- generate \
  --dll path/to/assembly.dll \
  --xml path/to/assembly.xml \
  --output ./output-dir \
  --overwrite
```

### Options

- `--dll <path>` - Path to the .NET assembly (required)
- `--xml <path>` - Path to the XML documentation file (optional)
- `--output <dir>` - Output directory for generated .hx files (required)
- `--namespace <ns>` - Filter to specific namespace(s) (optional, can repeat)
- `--recursive` - Include nested namespaces (default: true)
- `--overwrite` - Overwrite existing files (default: false)

### Download .NET reference assemblies

```bash
dotnet run -- download \
  --package NETStandard.Library.Ref \
  --version 2.1.0 \
  --output ./refs
```

### Generate .NET Standard 2.1 externs

```bash
# Download reference assemblies
dotnet run -- download --package NETStandard.Library.Ref --version 2.1.0 --output ./refs

# Generate externs for System.IO namespace
dotnet run -- generate \
  --dll ./refs/NETStandard.Library.Ref.2.1.0/ref/netstandard2.1/netstandard.dll \
  --xml ./refs/NETStandard.Library.Ref.2.1.0/ref/netstandard2.1/netstandard.xml \
  --output ./externs \
  --namespace System.IO \
  --overwrite

# Generate all externs
dotnet run -- generate \
  --dll ./refs/NETStandard.Library.Ref.2.1.0/ref/netstandard2.1/netstandard.dll \
  --xml ./refs/NETStandard.Library.Ref.2.1.0/ref/netstandard2.1/netstandard.xml \
  --output ./externs \
  --overwrite
```

## Output

The tool generates Haxe extern files with:

- Proper `@:native` annotations for .NET type names
- `@:overload` annotations for method overloads
- XML documentation as Haxe doc comments
- Correct type mappings (primitives, generics, arrays)
- Enum abstracts with proper underlying types
- Interface declarations
- Property accessors

## Type Mappings

| C# Type | Haxe Type |
|---------|-----------|
| `int`, `Int32` | `Int` |
| `long`, `Int64` | `haxe.Int64` |
| `float`, `Single` | `Single` |
| `double`, `Double` | `Float` |
| `bool` | `Bool` |
| `string` | `String` |
| `void` | `Void` |
| `object` | `Dynamic` |
| `T[]` | `cs.NativeArray<T>` |

## Multi-arity Types

Types with multiple generic arities (like `Action`, `Func`, `Tuple`) get numeric suffixes:

- `Action` - no type params
- `Action_1<T>` - one type param
- `Action_2<T1, T2>` - two type params

## Known Limitations

- Interface implementations don't include type parameters in the declaration
- Some generic type arguments in return types may be missing
- Span<T> and related ref struct types are skipped (not supported by Haxe)
