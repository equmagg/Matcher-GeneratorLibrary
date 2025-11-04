using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.Text;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using static Matcher.Generator.Helpers;

namespace MatcherGenerator
{
    [Generator]
    public sealed class ArithmeticOperatorsGenerator : ISourceGenerator
    {
        private const string AttributeSource =
@"using System;

namespace Matcher
{
    [AttributeUsage(AttributeTargets.Class | AttributeTargets.Struct, Inherited = false, AllowMultiple = true)]
    public sealed class AddableToAttribute : Attribute
    {
        public AddableToAttribute(Type other) { Other = other; }
        public Type Other { get; }
    }

    [AttributeUsage(AttributeTargets.Class | AttributeTargets.Struct, Inherited = false, AllowMultiple = true)]
    public sealed class AddableFromAttribute : Attribute
    {
        public AddableFromAttribute(Type other) { Other = other; }
        public Type Other { get; }
    }

    [AttributeUsage(AttributeTargets.Class | AttributeTargets.Struct, Inherited = false, AllowMultiple = true)]
    public sealed class SubstractableToAttribute : Attribute
    {
        public SubstractableToAttribute(Type other) { Other = other; }
        public Type Other { get; }
    }

    [AttributeUsage(AttributeTargets.Class | AttributeTargets.Struct, Inherited = false, AllowMultiple = true)]
    public sealed class SubstractableFromAttribute : Attribute
    {
        public SubstractableFromAttribute(Type other) { Other = other; }
        public Type Other { get; }
    }

    [AttributeUsage(AttributeTargets.Field | AttributeTargets.Property, Inherited = false, AllowMultiple = false)]
    public sealed class FixedAttribute : Attribute { }

    [AttributeUsage(AttributeTargets.Field | AttributeTargets.Property, Inherited = false, AllowMultiple = true)]
    public sealed class FieldAliasAttribute : Attribute
    {
        public FieldAliasAttribute(string alias) { Alias = alias; }
        public string Alias { get; }
    }
}
";
        public void Initialize(GeneratorInitializationContext context)
        {
            context.RegisterForPostInitialization(pi =>
            {
                pi.AddSource("ArithmeticAttributes.g.cs", SourceText.From(AttributeSource, Encoding.UTF8));
            });

            context.RegisterForSyntaxNotifications(() => new SyntaxReceiver());
        }
        public void Execute(GeneratorExecutionContext context)
        {
            if (context.SyntaxReceiver is not SyntaxReceiver rx)
                return;

            var compilation = context.Compilation;

            var addToAttr = compilation.GetTypeByMetadataName("Matcher.AddableToAttribute");
            var addFromAttr = compilation.GetTypeByMetadataName("Matcher.AddableFromAttribute");
            var subToAttr = compilation.GetTypeByMetadataName("Matcher.SubstractableToAttribute");
            var subFromAttr = compilation.GetTypeByMetadataName("Matcher.SubstractableFromAttribute");
            var fixedAttr = compilation.GetTypeByMetadataName("Matcher.FixedAttribute");
            foreach (var tds in rx.Candidates)
            {
                var model = compilation.GetSemanticModel(tds.SyntaxTree);
                if (model.GetDeclaredSymbol(tds) is not INamedTypeSymbol typeSym)
                    continue;
                if (typeSym.TypeKind is not (TypeKind.Class or TypeKind.Struct))
                    continue;
                var attrs = typeSym.GetAttributes();
                bool hasRelevant =
                    attrs.Any(a => SymbolEquals(a.AttributeClass, addToAttr) ||
                                   SymbolEquals(a.AttributeClass, addFromAttr) ||
                                   SymbolEquals(a.AttributeClass, subToAttr) ||
                                   SymbolEquals(a.AttributeClass, subFromAttr));

                if (!hasRelevant)
                    continue;
                if (!IsPartial(typeSym))
                {
                    ReportNotPartial(context, typeSym);
                    continue;
                }
                var requests = new List<OpRequest>();
                foreach (var a in attrs)
                {
                    var otherArg = a.ConstructorArguments.Length == 1 ? a.ConstructorArguments[0] : default;
                    if (otherArg.Value is not INamedTypeSymbol otherType)
                        continue;

                    if (SymbolEquals(a.AttributeClass, addToAttr))
                        requests.Add(new OpRequest(OperatorKind.Add, Handedness.LeftIsThis_To, otherType));
                    else if (SymbolEquals(a.AttributeClass, addFromAttr))
                        requests.Add(new OpRequest(OperatorKind.Add, Handedness.RightIsThis_From, otherType));
                    else if (SymbolEquals(a.AttributeClass, subToAttr))
                        requests.Add(new OpRequest(OperatorKind.Sub, Handedness.LeftIsThis_To, otherType));
                    else if (SymbolEquals(a.AttributeClass, subFromAttr))
                        requests.Add(new OpRequest(OperatorKind.Sub, Handedness.RightIsThis_From, otherType));
                }

                if (requests.Count == 0)
                    continue;
                var src = GenerateForType(typeSym, requests, compilation, fixedAttr);
                var hint = GetHintName(typeSym);
                context.AddSource(hint, SourceText.From(src, Encoding.UTF8));
            }
        }
        private static string GetHintName(INamedTypeSymbol typeSymbol)
        {
            var full = typeSymbol.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat);
            if (full.StartsWith("global::", StringComparison.Ordinal))
                full = full.Substring("global::".Length);

            var safe = full
                .Replace('.', '_')
                .Replace('+', '_')
                .Replace('<', '_')
                .Replace('>', '_')
                .Replace(',', '_');

            return $"{safe}.ArithmeticOperators.g.cs";
        }
        private static string GenerateForType(
            INamedTypeSymbol self,
            List<OpRequest> requests,
            Compilation compilation,
            INamedTypeSymbol? fixedAttr)
        {
            var ns = self.ContainingNamespace?.ToDisplayString() ?? string.Empty;
            var sb = new StringBuilder();
            sb.AppendLine("// <auto-generated/>");
            sb.AppendLine("using System;");

            if (!string.IsNullOrEmpty(ns))
            {
                sb.AppendLine($"namespace {ns}");
                sb.AppendLine("{");
            }
            foreach (var ct in GetContainingTypes(self))
            {
                if (!IsPartial(ct)) break;
                sb.AppendLine($"{GetAccessibilityKeyword(ct.DeclaredAccessibility)} partial {(ct.TypeKind == TypeKind.Struct ? "struct" : "class")} {ct.Name}");
                sb.AppendLine("{");
            }

            // generate partial
            sb.AppendLine($"{GetAccessibilityKeyword(self.DeclaredAccessibility)} partial {(self.TypeKind == TypeKind.Struct ? "struct" : "class")} {self.Name}");
            sb.AppendLine("{");
            // gather existing operators
            var existing = GetExistingOperatorPairs(self);

            foreach (var req in requests.Distinct())
            {
                var other = req.Other;
                if (other.TypeKind is not (TypeKind.Struct or TypeKind.Class))
                    continue;

                // already has an overload?
                if (existing.Contains((req.Kind, req.Hand, other, self)))
                    continue;

                // gather pairs
                var pairs = CollectMemberPairs(self, other, req, compilation, fixedAttr);
                if (pairs.Count == 0)
                    continue;

                EmitOperator(sb, self, other, req, pairs, compilation);
            }
            sb.AppendLine("}"); // end self

            var containing = GetContainingTypes(self);
            for (int i = 0; i < containing.Count; i++)
                sb.AppendLine("}");

            if (!string.IsNullOrEmpty(ns))
                sb.AppendLine("}");

            return sb.ToString();
        }
        private static void EmitOperator(
            StringBuilder sb,
            INamedTypeSymbol self,
            INamedTypeSymbol other,
            OpRequest req,
            List<MemberPair> pairs,
            Compilation compilation)
        {
            var selfFull = self.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat);
            var otherFull = other.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat);
            var opToken = req.Kind == OperatorKind.Add ? "+" : "-";
            var opTokenText = req.Kind == OperatorKind.Add ? "Adds" : "Subtracts";

            string signature;
            string initResult;
            string leftName, rightName;
            if (req.Hand == Handedness.LeftIsThis_To)
            {
                leftName = "__left";
                rightName = "__right";
                signature = $"public static {selfFull} operator {opToken}({selfFull} {leftName}, {otherFull} {rightName})";
                initResult = "__res = __left;";
            }
            else
            {
                leftName = "__left";
                rightName = "__right";
                signature = $"public static {selfFull} operator {opToken}({otherFull} {leftName}, {selfFull} {rightName})";
                initResult = "__res = __right;";
            }
            sb.AppendLine("    /// <summary>");
            sb.AppendLine($"    /// {opTokenText} matching numeric fields/properties by name, skipping members marked with [Matcher.Fixed].");
            sb.AppendLine("    /// </summary>");
            sb.AppendLine($"    {signature}");
            sb.AppendLine("    {");
            sb.AppendLine($"        var __res = {DefaultInit(self)};");
            sb.AppendLine($"        {initResult}");
            foreach (var p in pairs)
            {
                var leftExpr = $"__res.{p.SelfName}";
                var rightExpr = $"{(req.Hand == Handedness.LeftIsThis_To ? "__right" : "__left")}.{p.OtherName}";

                sb.AppendLine($"        {leftExpr} = {leftExpr} {opToken} {rightExpr};");
            }

            sb.AppendLine("        return __res;");
            sb.AppendLine("    }");
            sb.AppendLine();
        }
        private static string DefaultInit(INamedTypeSymbol t)
            => t.TypeKind == TypeKind.Struct
                ? $"default({t.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat)})" // no new object for struct
                : $"new {t.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat)}()";
        private static HashSet<(OperatorKind kind, Handedness hand, INamedTypeSymbol other, INamedTypeSymbol self)> GetExistingOperatorPairs(INamedTypeSymbol self)
        {
            var set = new HashSet<(OperatorKind, Handedness, INamedTypeSymbol, INamedTypeSymbol)>(new SymTupleComparer());
            foreach (var m in self.GetMembers())
            {
                if (m is not IMethodSymbol ms) continue;
                if (!ms.IsStatic) continue;
                if (ms.MethodKind != MethodKind.UserDefinedOperator) continue;
                if (ms.Parameters.Length != 2) continue;

                var kind = ms.Name switch
                {
                    "op_Addition" => OperatorKind.Add,
                    "op_Subtraction" => OperatorKind.Sub,
                    _ => (OperatorKind?)null
                };
                if (kind is null) continue;

                var p0 = ms.Parameters[0].Type as INamedTypeSymbol;
                var p1 = ms.Parameters[1].Type as INamedTypeSymbol;
                if (p0 is null || p1 is null) continue;

                if (SymbolEqualityComparer.Default.Equals(p0, self))
                    set.Add((kind.Value, Handedness.LeftIsThis_To, p1, self));
                else if (SymbolEqualityComparer.Default.Equals(p1, self))
                    set.Add((kind.Value, Handedness.RightIsThis_From, p0, self));
            }
            return set;
        }
        private static List<MemberPair> CollectMemberPairs(
            INamedTypeSymbol self,
            INamedTypeSymbol other,
            OpRequest req,
            Compilation compilation,
            INamedTypeSymbol? fixedAttr)
        {
            // modifiable?
            var selfMembers = self.GetMembers()
                .Where(m => !m.IsStatic)
                .Where(m =>
                {
                    if (m is IFieldSymbol f)
                        return !f.IsConst && !f.IsImplicitlyDeclared && !HasAttr(f, "Matcher.FixedAttribute");
                    if (m is IPropertySymbol p)
                        return !p.IsIndexer && p.SetMethod is not null && !HasAttr(p, "Matcher.FixedAttribute");
                    return false;
                })
                .ToDictionary(m => m.Name, StringComparer.Ordinal);

            var otherReadable = other.GetMembers()
                .Where(m => !m.IsStatic)
                .Where(m =>
                {
                    if (m is IFieldSymbol f)
                        return !f.IsImplicitlyDeclared && IsReadableFromAssembly(f, self);
                    if (m is IPropertySymbol p)
                        return !p.IsIndexer && p.GetMethod is not null && IsReadableFromAssembly(p, self);
                    return false;
                })
                .ToDictionary(m => m.Name, StringComparer.Ordinal);

            var pairs = new List<MemberPair>();
            foreach (var kvp in selfMembers)
            {
                string name = kvp.Key;
                ISymbol mSelf = kvp.Value;
                if (!otherReadable.TryGetValue(name, out var mOther))
                    continue;

                var tSelf = GetMemberType(mSelf);
                var tOther = GetMemberType(mOther);
                if (tSelf is null || tOther is null)
                    continue;

                if (!IsNumeric(tSelf) || !IsNumeric(tOther))
                    continue;

                var conv = compilation.ClassifyConversion(tOther, tSelf);
                if (!conv.Exists || !conv.IsImplicit)
                    continue;

                pairs.Add(new MemberPair(name, name));
            }

            return pairs;
        }
        private static void ReportNotPartial(GeneratorExecutionContext context, INamedTypeSymbol type)
        {
            var desc = new DiagnosticDescriptor(
                id: "MATH001",
                title: "Type must be partial to generate operators",
                messageFormat: "Type '{0}' must be declared as partial to generate operator overloads.",
                category: "Matcher.Arithmetic",
                DiagnosticSeverity.Info,
                isEnabledByDefault: true);
            context.ReportDiagnostic(Diagnostic.Create(desc, Location.None, type.ToDisplayString()));
        }
       
        private enum OperatorKind { Add, Sub }
        private enum Handedness { LeftIsThis_To, RightIsThis_From }
        private readonly struct MemberPair
        {
            public string SelfName { get; }
            public string OtherName { get; }
            public MemberPair(string selfName, string otherName)
            {
                SelfName = selfName;
                OtherName = otherName;
            }
        }
        private readonly struct OpRequest : IEquatable<OpRequest>
        {
            public OperatorKind Kind { get; }
            public Handedness Hand { get; }
            public INamedTypeSymbol Other { get; }

            public OpRequest(OperatorKind kind, Handedness hand, INamedTypeSymbol other)
            {
                Kind = kind; Hand = hand; Other = other;
            }

            public bool Equals(OpRequest other) =>
                Kind == other.Kind &&
                Hand == other.Hand &&
                SymbolEqualityComparer.Default.Equals(Other, other.Other);

            public override bool Equals(object? obj) => obj is OpRequest o && Equals(o);
            public override int GetHashCode()
            {
                unchecked
                {
                    int h = (int)Kind;
                    h = (h * 397) ^ (int)Hand;
                    h = (h * 397) ^ SymbolEqualityComparer.Default.GetHashCode(Other);
                    return h;
                }
            }
        }
        
        private sealed class SymTupleComparer : IEqualityComparer<(OperatorKind, Handedness, INamedTypeSymbol, INamedTypeSymbol)>
        {
            public bool Equals((OperatorKind, Handedness, INamedTypeSymbol, INamedTypeSymbol) x,
                               (OperatorKind, Handedness, INamedTypeSymbol, INamedTypeSymbol) y)
            {
                return x.Item1 == y.Item1 && x.Item2 == y.Item2 &&
                       SymbolEqualityComparer.Default.Equals(x.Item3, y.Item3) &&
                       SymbolEqualityComparer.Default.Equals(x.Item4, y.Item4);
            }
            public int GetHashCode((OperatorKind, Handedness, INamedTypeSymbol, INamedTypeSymbol) obj)
            {
                unchecked
                {
                    int h = (int)obj.Item1;
                    h = (h * 397) ^ (int)obj.Item2;
                    h = (h * 397) ^ SymbolEqualityComparer.Default.GetHashCode(obj.Item3);
                    h = (h * 397) ^ SymbolEqualityComparer.Default.GetHashCode(obj.Item4);
                    return h;
                }
            }
        }

        private sealed class SyntaxReceiver : ISyntaxReceiver
        {
            public List<TypeDeclarationSyntax> Candidates { get; } = new();

            public void OnVisitSyntaxNode(SyntaxNode node)
            {
                if (node is TypeDeclarationSyntax tds &&
                    (tds is ClassDeclarationSyntax or StructDeclarationSyntax) &&
                    tds.AttributeLists.Count > 0)
                {
                    Candidates.Add(tds);
                }
            }
        }

    }
}
