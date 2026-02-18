namespace ExternGenerator.Models;

public enum TypeKind
{
    Class,
    Interface,
    Struct,
    Enum,
    Delegate
}

public class TypeInfo
{
    public string Name { get; set; } = "";
    public string Namespace { get; set; } = "";
    public string FullName => string.IsNullOrEmpty(Namespace) ? Name : $"{Namespace}.{Name}";
    public TypeKind Kind { get; set; }
    public bool IsAbstract { get; set; }
    public bool IsSealed { get; set; }
    public bool IsStatic { get; set; }
    public int GenericParameterCount { get; set; }
    public List<string> GenericParameters { get; set; } = new();
    public string? BaseType { get; set; }
    public List<string> Interfaces { get; set; } = new();
    public List<MethodInfo> Methods { get; set; } = new();
    public List<PropertyInfo> Properties { get; set; } = new();
    public List<FieldInfo> Fields { get; set; } = new();
    public List<EventInfo> Events { get; set; } = new();
    public List<TypeInfo> NestedTypes { get; set; } = new();
    public string? XmlDoc { get; set; }

    // For enums with [Flags] attribute
    public bool IsFlags { get; set; }

    // For nested types
    public string? DeclaringType { get; set; }

    // The original .NET name with backtick notation (e.g., "Dictionary`2")
    public string MetadataName { get; set; } = "";
}

public class MethodInfo
{
    public string Name { get; set; } = "";
    public bool IsStatic { get; set; }
    public bool IsConstructor { get; set; }
    public bool IsAbstract { get; set; }
    public bool IsVirtual { get; set; }
    public string ReturnType { get; set; } = "Void";
    public List<ParameterInfo> Parameters { get; set; } = new();
    public List<string> GenericParameters { get; set; } = new();
    public string? XmlDoc { get; set; }
}

public class ParameterInfo
{
    public string Name { get; set; } = "";
    public string Type { get; set; } = "";
    public bool IsOut { get; set; }
    public bool IsRef { get; set; }
    public bool IsParams { get; set; }
    public bool HasDefault { get; set; }
    public object? DefaultValue { get; set; }

    // Alias for HasDefault used in extern generation
    public bool IsOptional => HasDefault;
}

public class PropertyInfo
{
    public string Name { get; set; } = "";
    public string Type { get; set; } = "";
    public bool IsStatic { get; set; }
    public bool HasGetter { get; set; }
    public bool HasSetter { get; set; }
    public bool IsIndexer { get; set; }
    public List<ParameterInfo> IndexerParameters { get; set; } = new();
    public string? XmlDoc { get; set; }

    // Alias for IndexerParameters
    public List<ParameterInfo> Parameters => IndexerParameters;
}

public class FieldInfo
{
    public string Name { get; set; } = "";
    public string Type { get; set; } = "";
    public bool IsStatic { get; set; }
    public bool IsReadOnly { get; set; }
    public bool IsConst { get; set; }
    public object? ConstValue { get; set; }
    public string? XmlDoc { get; set; }

    // Alias for IsConst (in .NET metadata, const fields have the Literal flag)
    public bool IsLiteral => IsConst;
}

public class EventInfo
{
    public string Name { get; set; } = "";
    public string Type { get; set; } = "";
    public bool IsStatic { get; set; }
    public string? XmlDoc { get; set; }
}
