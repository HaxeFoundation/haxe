using System.Collections.Immutable;
using System.Reflection;
using System.Reflection.Metadata;
using System.Reflection.PortableExecutable;
using ExternGenerator.Models;
using MethodInfo = ExternGenerator.Models.MethodInfo;
using ParameterInfo = ExternGenerator.Models.ParameterInfo;
using PropertyInfo = ExternGenerator.Models.PropertyInfo;
using FieldInfo = ExternGenerator.Models.FieldInfo;
using EventInfo = ExternGenerator.Models.EventInfo;
using TypeInfo = ExternGenerator.Models.TypeInfo;

namespace ExternGenerator.Analysis;

public class AssemblyAnalyzer
{
    private MetadataReader _reader = null!;
    private XmlDocParser? _xmlDoc;

    public List<TypeInfo> AnalyzeAssembly(string assemblyPath, string? xmlPath = null)
    {
        var types = new List<TypeInfo>();

        if (xmlPath != null && File.Exists(xmlPath))
        {
            _xmlDoc = new XmlDocParser();
            _xmlDoc.Load(xmlPath);
        }

        using var stream = File.OpenRead(assemblyPath);
        using var peReader = new PEReader(stream);
        _reader = peReader.GetMetadataReader();

        foreach (var typeDefHandle in _reader.TypeDefinitions)
        {
            var typeDef = _reader.GetTypeDefinition(typeDefHandle);

            // Skip non-public types
            var visibility = typeDef.Attributes & TypeAttributes.VisibilityMask;
            if (visibility != TypeAttributes.Public && visibility != TypeAttributes.NestedPublic)
                continue;

            // Skip compiler-generated types
            var name = _reader.GetString(typeDef.Name);
            if (name.StartsWith("<") || name.Contains("__"))
                continue;

            // For nested types, build the full nested name
            string fullTypeName = name;
            string ns = _reader.GetString(typeDef.Namespace);
            var declaringType = typeDef.GetDeclaringType();
            if (!declaringType.IsNil)
            {
                // Build the full nested type name by traversing up to the root type
                var parentNames = new List<string>();
                var currentType = declaringType;
                while (!currentType.IsNil)
                {
                    var parentDef = _reader.GetTypeDefinition(currentType);
                    var parentName = _reader.GetString(parentDef.Name);
                    parentNames.Insert(0, parentName);

                    // Get namespace from parent if this is the root
                    var grandparent = parentDef.GetDeclaringType();
                    if (grandparent.IsNil)
                    {
                        ns = _reader.GetString(parentDef.Namespace);
                    }
                    currentType = grandparent;
                }
                fullTypeName = string.Join("+", parentNames) + "+" + name;
            }

            var typeInfo = AnalyzeType(typeDef, typeDefHandle, fullTypeName, ns);
            if (typeInfo != null)
            {
                types.Add(typeInfo);
            }
        }

        // Post-process: remove properties that would conflict with base class properties
        RemoveConflictingProperties(types);

        return types;
    }

    /// <summary>
    /// Removes properties from derived types that would conflict with base type properties.
    /// This handles cases like Type.Module hiding MemberInfo.Module with 'new' keyword.
    /// </summary>
    private void RemoveConflictingProperties(List<TypeInfo> types)
    {
        // Build a dictionary of type metadata name -> TypeInfo
        // Use metadata name (e.g., "System.Action`2") to handle multi-arity types
        var typeDict = new Dictionary<string, TypeInfo>();
        foreach (var type in types)
        {
            string key = string.IsNullOrEmpty(type.Namespace)
                ? type.MetadataName
                : $"{type.Namespace}.{type.MetadataName}";
            typeDict.TryAdd(key, type);
        }

        // For each type, collect property names from all ancestors
        foreach (var type in types)
        {
            var ancestorPropNames = new HashSet<string>();
            string? currentBase = type.BaseType;

            while (!string.IsNullOrEmpty(currentBase) &&
                   currentBase != "System.Object" &&
                   currentBase != "System.ValueType")
            {
                // Base type might include generic args like "System.Delegate"
                // or "System.Collections.Generic.List`1<T>"
                string baseKey = currentBase.Contains('<')
                    ? currentBase.Substring(0, currentBase.IndexOf('<'))
                    : currentBase;

                if (typeDict.TryGetValue(baseKey, out var baseType))
                {
                    foreach (var prop in baseType.Properties)
                    {
                        ancestorPropNames.Add(prop.Name);
                    }
                    currentBase = baseType.BaseType;
                }
                else
                {
                    break;
                }
            }

            // Remove properties that conflict with ancestor properties
            if (ancestorPropNames.Any())
            {
                type.Properties = type.Properties
                    .Where(p => !ancestorPropNames.Contains(p.Name))
                    .ToList();
            }
        }
    }

    private TypeInfo? AnalyzeType(TypeDefinition typeDef, TypeDefinitionHandle handle, string fullTypeName, string ns)
    {
        var typeInfo = new TypeInfo
        {
            Name = GetHaxeTypeName(fullTypeName),
            MetadataName = fullTypeName,
            Namespace = ns,
            IsAbstract = (typeDef.Attributes & TypeAttributes.Abstract) != 0,
            IsSealed = (typeDef.Attributes & TypeAttributes.Sealed) != 0,
        };

        // Determine type kind
        typeInfo.Kind = DetermineTypeKind(typeDef);

        // Check for [Flags] attribute on enums
        if (typeInfo.Kind == TypeKind.Enum)
        {
            typeInfo.IsFlags = HasFlagsAttribute(typeDef);
        }

        // Get generic parameters
        foreach (var gpHandle in typeDef.GetGenericParameters())
        {
            var gp = _reader.GetGenericParameter(gpHandle);
            typeInfo.GenericParameters.Add(_reader.GetString(gp.Name));
        }
        typeInfo.GenericParameterCount = typeInfo.GenericParameters.Count;

        // Get base type
        if (!typeDef.BaseType.IsNil)
        {
            typeInfo.BaseType = GetTypeName(typeDef.BaseType);
        }

        // Get interfaces (with generic context for proper type parameter resolution)
        var typeContext = new GenericContext(typeInfo.GenericParameters);
        foreach (var ifaceHandle in typeDef.GetInterfaceImplementations())
        {
            var iface = _reader.GetInterfaceImplementation(ifaceHandle);
            var ifaceName = GetTypeName(iface.Interface, typeContext);
            if (!string.IsNullOrEmpty(ifaceName))
            {
                typeInfo.Interfaces.Add(ifaceName);
            }
        }

        // Get members based on type kind
        if (typeInfo.Kind == TypeKind.Enum)
        {
            AnalyzeEnumFields(typeDef, typeInfo);
        }
        else
        {
            AnalyzeMethods(typeDef, typeInfo);
            AnalyzeProperties(typeDef, typeInfo);
            AnalyzeFields(typeDef, typeInfo);
            AnalyzeEvents(typeDef, typeInfo);
        }

        // Note: Nested types are now handled in the main loop (AnalyzeAssembly)
        // so they get generated as separate Haxe files

        // Get XML documentation
        typeInfo.XmlDoc = _xmlDoc?.GetTypeDoc(typeInfo.FullName)?.Summary;

        return typeInfo;
    }

    private TypeKind DetermineTypeKind(TypeDefinition typeDef)
    {
        var attributes = typeDef.Attributes;

        if ((attributes & TypeAttributes.Interface) != 0)
            return TypeKind.Interface;

        if (!typeDef.BaseType.IsNil)
        {
            var baseName = GetTypeName(typeDef.BaseType);
            if (baseName == "System.Enum")
                return TypeKind.Enum;
            if (baseName == "System.ValueType")
                return TypeKind.Struct;
            if (baseName == "System.MulticastDelegate" || baseName == "System.Delegate")
                return TypeKind.Delegate;
        }

        return TypeKind.Class;
    }

    private bool HasFlagsAttribute(TypeDefinition typeDef)
    {
        foreach (var attrHandle in typeDef.GetCustomAttributes())
        {
            var attr = _reader.GetCustomAttribute(attrHandle);
            var attrCtor = attr.Constructor;

            string? attrTypeName = null;
            if (attrCtor.Kind == HandleKind.MemberReference)
            {
                var memberRef = _reader.GetMemberReference((MemberReferenceHandle)attrCtor);
                var parent = memberRef.Parent;
                if (parent.Kind == HandleKind.TypeReference)
                {
                    var typeRef = _reader.GetTypeReference((TypeReferenceHandle)parent);
                    var ns = _reader.GetString(typeRef.Namespace);
                    var name = _reader.GetString(typeRef.Name);
                    attrTypeName = $"{ns}.{name}";
                }
            }
            else if (attrCtor.Kind == HandleKind.MethodDefinition)
            {
                var methodDef = _reader.GetMethodDefinition((MethodDefinitionHandle)attrCtor);
                var typeDefHandle = methodDef.GetDeclaringType();
                var attrTypeDef = _reader.GetTypeDefinition(typeDefHandle);
                var ns = _reader.GetString(attrTypeDef.Namespace);
                var name = _reader.GetString(attrTypeDef.Name);
                attrTypeName = $"{ns}.{name}";
            }

            if (attrTypeName == "System.FlagsAttribute")
                return true;
        }
        return false;
    }

    private void AnalyzeMethods(TypeDefinition typeDef, TypeInfo typeInfo)
    {
        var methods = new List<MethodInfo>();

        foreach (var methodHandle in typeDef.GetMethods())
        {
            var methodDef = _reader.GetMethodDefinition(methodHandle);
            var methodName = _reader.GetString(methodDef.Name);

            // Skip private methods
            var visibility = methodDef.Attributes & MethodAttributes.MemberAccessMask;
            if (visibility != MethodAttributes.Public)
                continue;

            // Skip property accessors and event handlers (handled separately)
            if (methodName.StartsWith("get_") || methodName.StartsWith("set_") ||
                methodName.StartsWith("add_") || methodName.StartsWith("remove_"))
                continue;

            var methodInfo = new MethodInfo
            {
                Name = methodName == ".ctor" ? "new" : methodName,
                IsConstructor = methodName == ".ctor",
                IsStatic = (methodDef.Attributes & MethodAttributes.Static) != 0,
                IsAbstract = (methodDef.Attributes & MethodAttributes.Abstract) != 0,
                IsVirtual = (methodDef.Attributes & MethodAttributes.Virtual) != 0,
            };

            // Get generic parameters
            foreach (var gpHandle in methodDef.GetGenericParameters())
            {
                var gp = _reader.GetGenericParameter(gpHandle);
                methodInfo.GenericParameters.Add(_reader.GetString(gp.Name));
            }

            // Get signature - pass generic context for proper parameter name resolution
            var context = new GenericContext(typeInfo.GenericParameters, methodInfo.GenericParameters);
            var signature = methodDef.DecodeSignature(new TypeNameProvider(_reader), context);

            methodInfo.ReturnType = signature.ReturnType;

            // Get parameters
            var paramIndex = 0;
            foreach (var paramHandle in methodDef.GetParameters())
            {
                var param = _reader.GetParameter(paramHandle);
                if (param.SequenceNumber == 0) continue; // Return value

                var paramInfo = new ParameterInfo
                {
                    Name = _reader.GetString(param.Name),
                    IsOut = (param.Attributes & ParameterAttributes.Out) != 0,
                    HasDefault = (param.Attributes & ParameterAttributes.HasDefault) != 0,
                };

                if (paramIndex < signature.ParameterTypes.Length)
                {
                    paramInfo.Type = signature.ParameterTypes[paramIndex];
                }

                // Check for ref/out in type
                if (paramInfo.Type.StartsWith("ref "))
                {
                    paramInfo.IsRef = true;
                    paramInfo.Type = paramInfo.Type.Substring(4);
                }
                else if (paramInfo.Type.StartsWith("out "))
                {
                    paramInfo.IsOut = true;
                    paramInfo.Type = paramInfo.Type.Substring(4);
                }

                methodInfo.Parameters.Add(paramInfo);
                paramIndex++;
            }

            methods.Add(methodInfo);
        }

        typeInfo.Methods = methods;
    }

    private void AnalyzeProperties(TypeDefinition typeDef, TypeInfo typeInfo)
    {
        foreach (var propHandle in typeDef.GetProperties())
        {
            var propDef = _reader.GetPropertyDefinition(propHandle);
            var propName = _reader.GetString(propDef.Name);

            // Check if accessors are public
            var accessors = propDef.GetAccessors();
            bool hasPublicGetter = false;
            bool hasPublicSetter = false;
            bool isStatic = false;

            bool isOverride = false;

            if (!accessors.Getter.IsNil)
            {
                var getter = _reader.GetMethodDefinition(accessors.Getter);
                hasPublicGetter = (getter.Attributes & MethodAttributes.Public) != 0;
                isStatic = (getter.Attributes & MethodAttributes.Static) != 0;
                bool getterIsVirtual = (getter.Attributes & MethodAttributes.Virtual) != 0;
                bool getterIsNewSlot = (getter.Attributes & MethodAttributes.NewSlot) != 0;
                if (getterIsVirtual && !getterIsNewSlot)
                    isOverride = true;
            }

            if (!accessors.Setter.IsNil)
            {
                var setter = _reader.GetMethodDefinition(accessors.Setter);
                hasPublicSetter = (setter.Attributes & MethodAttributes.Public) != 0;
                if (!isStatic)
                    isStatic = (setter.Attributes & MethodAttributes.Static) != 0;
                bool setterIsVirtual = (setter.Attributes & MethodAttributes.Virtual) != 0;
                bool setterIsNewSlot = (setter.Attributes & MethodAttributes.NewSlot) != 0;
                if (setterIsVirtual && !setterIsNewSlot)
                    isOverride = true;
            }

            if (!hasPublicGetter && !hasPublicSetter)
                continue;

            // Skip override properties - they're already defined in the base class
            // (Virtual without NewSlot = override, Virtual with NewSlot = new virtual property)
            // Note: Properties hiding base class properties with 'new' are handled in post-processing
            if (isOverride)
                continue;

            var context = new GenericContext(typeInfo.GenericParameters);
            var signature = propDef.DecodeSignature(new TypeNameProvider(_reader), context);

            var propInfo = new PropertyInfo
            {
                Name = propName,
                Type = signature.ReturnType,
                IsStatic = isStatic,
                HasGetter = hasPublicGetter,
                HasSetter = hasPublicSetter,
                IsIndexer = propName == "Item" && signature.ParameterTypes.Length > 0,
            };

            // Get indexer parameters
            if (propInfo.IsIndexer)
            {
                for (int i = 0; i < signature.ParameterTypes.Length; i++)
                {
                    propInfo.IndexerParameters.Add(new ParameterInfo
                    {
                        Name = $"index{i}",
                        Type = signature.ParameterTypes[i]
                    });
                }
            }

            typeInfo.Properties.Add(propInfo);
        }
    }

    private void AnalyzeFields(TypeDefinition typeDef, TypeInfo typeInfo)
    {
        foreach (var fieldHandle in typeDef.GetFields())
        {
            var fieldDef = _reader.GetFieldDefinition(fieldHandle);

            // Skip non-public fields
            var visibility = fieldDef.Attributes & FieldAttributes.FieldAccessMask;
            if (visibility != FieldAttributes.Public)
                continue;

            var fieldName = _reader.GetString(fieldDef.Name);
            var context = new GenericContext(typeInfo.GenericParameters);
            var signature = fieldDef.DecodeSignature(new TypeNameProvider(_reader), context);

            var fieldInfo = new FieldInfo
            {
                Name = fieldName,
                Type = signature,
                IsStatic = (fieldDef.Attributes & FieldAttributes.Static) != 0,
                IsReadOnly = (fieldDef.Attributes & FieldAttributes.InitOnly) != 0,
                IsConst = (fieldDef.Attributes & FieldAttributes.Literal) != 0,
            };

            if (fieldInfo.IsConst)
            {
                var constantHandle = fieldDef.GetDefaultValue();
                if (!constantHandle.IsNil)
                {
                    var constant = _reader.GetConstant(constantHandle);
                    var blob = _reader.GetBlobReader(constant.Value);
                    fieldInfo.ConstValue = ReadConstant(blob, constant.TypeCode);
                }
            }

            typeInfo.Fields.Add(fieldInfo);
        }
    }

    private void AnalyzeEnumFields(TypeDefinition typeDef, TypeInfo typeInfo)
    {
        foreach (var fieldHandle in typeDef.GetFields())
        {
            var fieldDef = _reader.GetFieldDefinition(fieldHandle);
            var fieldName = _reader.GetString(fieldDef.Name);

            // Skip the special "value__" field
            if (fieldName == "value__") continue;

            // Skip non-public fields
            var visibility = fieldDef.Attributes & FieldAttributes.FieldAccessMask;
            if (visibility != FieldAttributes.Public)
                continue;

            var fieldInfo = new FieldInfo
            {
                Name = fieldName,
                Type = "Int",
                IsStatic = true,
                IsConst = true,
            };

            var constantHandle = fieldDef.GetDefaultValue();
            if (!constantHandle.IsNil)
            {
                var constant = _reader.GetConstant(constantHandle);
                var blob = _reader.GetBlobReader(constant.Value);
                fieldInfo.ConstValue = ReadEnumValue(blob);
            }

            typeInfo.Fields.Add(fieldInfo);
        }
    }

    private void AnalyzeEvents(TypeDefinition typeDef, TypeInfo typeInfo)
    {
        foreach (var eventHandle in typeDef.GetEvents())
        {
            var eventDef = _reader.GetEventDefinition(eventHandle);
            var eventName = _reader.GetString(eventDef.Name);

            // Check if add/remove are public
            var accessors = eventDef.GetAccessors();
            bool isPublic = false;
            bool isStatic = false;

            if (!accessors.Adder.IsNil)
            {
                var adder = _reader.GetMethodDefinition(accessors.Adder);
                isPublic = (adder.Attributes & MethodAttributes.Public) != 0;
                isStatic = (adder.Attributes & MethodAttributes.Static) != 0;
            }

            if (!isPublic)
                continue;

            var eventType = GetTypeName(eventDef.Type);

            typeInfo.Events.Add(new EventInfo
            {
                Name = eventName,
                Type = eventType,
                IsStatic = isStatic,
            });
        }
    }

    private string GetTypeName(EntityHandle handle, GenericContext? context = null)
    {
        if (handle.IsNil) return "";

        switch (handle.Kind)
        {
            case HandleKind.TypeDefinition:
                var typeDef = _reader.GetTypeDefinition((TypeDefinitionHandle)handle);
                var defNs = _reader.GetString(typeDef.Namespace);
                var defName = _reader.GetString(typeDef.Name);
                return string.IsNullOrEmpty(defNs) ? defName : $"{defNs}.{defName}";

            case HandleKind.TypeReference:
                var typeRef = _reader.GetTypeReference((TypeReferenceHandle)handle);
                var refNs = _reader.GetString(typeRef.Namespace);
                var refName = _reader.GetString(typeRef.Name);
                return string.IsNullOrEmpty(refNs) ? refName : $"{refNs}.{refName}";

            case HandleKind.TypeSpecification:
                var typeSpec = _reader.GetTypeSpecification((TypeSpecificationHandle)handle);
                return typeSpec.DecodeSignature(new TypeNameProvider(_reader), context);

            default:
                return "";
        }
    }

    private string GetHaxeTypeName(string metadataName)
    {
        // Handle nested types: replace + with _ and strip generic arity suffixes
        // E.g., "ConfiguredTaskAwaitable`1+ConfiguredTaskAwaiter" -> "ConfiguredTaskAwaitable_ConfiguredTaskAwaiter"
        // The arity suffix will be added back by TypeMapper based on multi-arity detection
        var result = new System.Text.StringBuilder();
        var parts = metadataName.Split('+');

        for (int i = 0; i < parts.Length; i++)
        {
            if (i > 0) result.Append('_');
            var part = parts[i];
            // Remove generic arity suffix from each part
            var backtickIndex = part.IndexOf('`');
            result.Append(backtickIndex >= 0 ? part.Substring(0, backtickIndex) : part);
        }

        return result.ToString();
    }

    private object? ReadConstant(BlobReader reader, ConstantTypeCode typeCode)
    {
        try
        {
            return typeCode switch
            {
                ConstantTypeCode.Boolean => reader.ReadBoolean(),
                ConstantTypeCode.Char => reader.ReadChar(),
                ConstantTypeCode.SByte => reader.ReadSByte(),
                ConstantTypeCode.Byte => reader.ReadByte(),
                ConstantTypeCode.Int16 => reader.ReadInt16(),
                ConstantTypeCode.UInt16 => reader.ReadUInt16(),
                ConstantTypeCode.Int32 => reader.ReadInt32(),
                ConstantTypeCode.UInt32 => reader.ReadUInt32(),
                ConstantTypeCode.Int64 => reader.ReadInt64(),
                ConstantTypeCode.UInt64 => reader.ReadUInt64(),
                ConstantTypeCode.Single => reader.ReadSingle(),
                ConstantTypeCode.Double => reader.ReadDouble(),
                ConstantTypeCode.String => reader.ReadUTF16(reader.Length),
                ConstantTypeCode.NullReference => null,
                _ => null
            };
        }
        catch
        {
            return null;
        }
    }

    private object? ReadEnumValue(BlobReader reader)
    {
        // Try reading as int32 by default
        try
        {
            return reader.ReadInt32();
        }
        catch
        {
            return null;
        }
    }
}

// Type signature decoder that returns type names as strings
class TypeNameProvider : ISignatureTypeProvider<string, object?>
{
    private readonly MetadataReader _reader;

    public TypeNameProvider(MetadataReader reader)
    {
        _reader = reader;
    }

    public string GetPrimitiveType(PrimitiveTypeCode typeCode) => typeCode switch
    {
        PrimitiveTypeCode.Void => "System.Void",
        PrimitiveTypeCode.Boolean => "System.Boolean",
        PrimitiveTypeCode.Char => "System.Char",
        PrimitiveTypeCode.SByte => "System.SByte",
        PrimitiveTypeCode.Byte => "System.Byte",
        PrimitiveTypeCode.Int16 => "System.Int16",
        PrimitiveTypeCode.UInt16 => "System.UInt16",
        PrimitiveTypeCode.Int32 => "System.Int32",
        PrimitiveTypeCode.UInt32 => "System.UInt32",
        PrimitiveTypeCode.Int64 => "System.Int64",
        PrimitiveTypeCode.UInt64 => "System.UInt64",
        PrimitiveTypeCode.Single => "System.Single",
        PrimitiveTypeCode.Double => "System.Double",
        PrimitiveTypeCode.String => "System.String",
        PrimitiveTypeCode.Object => "System.Object",
        PrimitiveTypeCode.IntPtr => "System.IntPtr",
        PrimitiveTypeCode.UIntPtr => "System.UIntPtr",
        PrimitiveTypeCode.TypedReference => "System.TypedReference",
        _ => "System.Object"
    };

    public string GetTypeFromDefinition(MetadataReader reader, TypeDefinitionHandle handle, byte rawTypeKind)
    {
        var typeDef = reader.GetTypeDefinition(handle);
        var ns = reader.GetString(typeDef.Namespace);
        var name = reader.GetString(typeDef.Name);

        // Check if this is a nested type
        var declaringType = typeDef.GetDeclaringType();
        if (!declaringType.IsNil)
        {
            var parentName = GetTypeFromDefinition(reader, declaringType, rawTypeKind);
            // Use + for nested type separator (will be converted by TypeMapper)
            return $"{parentName}+{name}";
        }

        return FormatTypeName(ns, name);
    }

    public string GetTypeFromReference(MetadataReader reader, TypeReferenceHandle handle, byte rawTypeKind)
    {
        var typeRef = reader.GetTypeReference(handle);
        var ns = reader.GetString(typeRef.Namespace);
        var name = reader.GetString(typeRef.Name);

        // Check if this is a nested type (ResolutionScope is another TypeReference)
        if (typeRef.ResolutionScope.Kind == HandleKind.TypeReference)
        {
            var parentHandle = (TypeReferenceHandle)typeRef.ResolutionScope;
            var parentName = GetTypeFromReference(reader, parentHandle, rawTypeKind);
            // Use + for nested type separator (will be converted by TypeMapper)
            return $"{parentName}+{name}";
        }

        return FormatTypeName(ns, name);
    }

    private string FormatTypeName(string ns, string name)
    {
        // Return the full .NET type name - conversion to Haxe is done by TypeMapper
        if (string.IsNullOrEmpty(ns))
            return name;

        return $"{ns}.{name}";
    }

    public string GetTypeFromSpecification(MetadataReader reader, object? genericContext, TypeSpecificationHandle handle, byte rawTypeKind)
    {
        var typeSpec = reader.GetTypeSpecification(handle);
        return typeSpec.DecodeSignature(this, genericContext);
    }

    public string GetSZArrayType(string elementType) => $"{elementType}[]";

    public string GetArrayType(string elementType, ArrayShape shape) => $"{elementType}[]";

    public string GetByReferenceType(string elementType) => $"ref {elementType}";

    public string GetPointerType(string elementType) => $"{elementType}*";

    public string GetGenericInstantiation(string genericType, ImmutableArray<string> typeArguments)
    {
        if (typeArguments.Length == 0)
            return genericType;

        return $"{genericType}<{string.Join(", ", typeArguments)}>";
    }

    public string GetGenericMethodParameter(object? genericContext, int index)
    {
        // Check if we have method generic parameters in context
        if (genericContext is GenericContext ctx && ctx.MethodParams != null && index < ctx.MethodParams.Count)
        {
            return ctx.MethodParams[index];
        }
        return $"M{index}";
    }

    public string GetGenericTypeParameter(object? genericContext, int index)
    {
        // Check if we have type generic parameters in context
        if (genericContext is GenericContext ctx && ctx.TypeParams != null && index < ctx.TypeParams.Count)
        {
            return ctx.TypeParams[index];
        }
        return $"T{index}";
    }

    public string GetFunctionPointerType(MethodSignature<string> signature) => "Dynamic";

    public string GetModifiedType(string modifier, string unmodifiedType, bool isRequired) => unmodifiedType;

    public string GetPinnedType(string elementType) => elementType;
}

// Context for resolving generic type parameters
class GenericContext
{
    public List<string>? TypeParams { get; set; }
    public List<string>? MethodParams { get; set; }

    public GenericContext(List<string>? typeParams = null, List<string>? methodParams = null)
    {
        TypeParams = typeParams;
        MethodParams = methodParams;
    }

    public GenericContext WithMethodParams(List<string>? methodParams)
    {
        return new GenericContext(TypeParams, methodParams);
    }
}
