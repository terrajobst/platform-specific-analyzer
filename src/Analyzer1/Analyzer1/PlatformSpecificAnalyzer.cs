using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.Diagnostics;
using Microsoft.CodeAnalysis.Operations;

namespace Analyzer1
{
    [DiagnosticAnalyzer(LanguageNames.CSharp, LanguageNames.VisualBasic)]
    public sealed class PlatformSpecificAnalyzer : DiagnosticAnalyzer
    {
        private static readonly DiagnosticDescriptor _pc0001 = new DiagnosticDescriptor(
            id: @"PC0001",
            title: new LocalizableResourceString(nameof(Resources.PC0001Title), Resources.ResourceManager, typeof(Resources)),
            messageFormat: new LocalizableResourceString(nameof(Resources.PC0001MessageFormat), Resources.ResourceManager, typeof(Resources)),
            category: @"Portability",
            defaultSeverity: DiagnosticSeverity.Warning,
            isEnabledByDefault: true,
            description: new LocalizableResourceString(nameof(Resources.PC0001Description), Resources.ResourceManager, typeof(Resources))
        );

        private static readonly DiagnosticDescriptor _pc0002 = new DiagnosticDescriptor(
            id: @"PC0002",
            title: new LocalizableResourceString(nameof(Resources.PC0002Title), Resources.ResourceManager, typeof(Resources)),
            messageFormat: new LocalizableResourceString(nameof(Resources.PC0002MessageFormat), Resources.ResourceManager, typeof(Resources)),
            category: @"Portability",
            defaultSeverity: DiagnosticSeverity.Warning,
            isEnabledByDefault: true,
            description: new LocalizableResourceString(nameof(Resources.PC0002Description), Resources.ResourceManager, typeof(Resources))
        );

        private static readonly DiagnosticDescriptor _pc0003 = new DiagnosticDescriptor(
            id: @"PC0003",
            title: new LocalizableResourceString(nameof(Resources.PC0003Title), Resources.ResourceManager, typeof(Resources)),
            messageFormat: new LocalizableResourceString(nameof(Resources.PC0003MessageFormat), Resources.ResourceManager, typeof(Resources)),
            category: @"Portability",
            defaultSeverity: DiagnosticSeverity.Warning,
            isEnabledByDefault: true,
            description: new LocalizableResourceString(nameof(Resources.PC0003Description), Resources.ResourceManager, typeof(Resources))
        );

        public override ImmutableArray<DiagnosticDescriptor> SupportedDiagnostics => ImmutableArray.Create(_pc0001, _pc0002, _pc0003);

        public override void Initialize(AnalysisContext context)
        {
            context.ConfigureGeneratedCodeAnalysis(GeneratedCodeAnalysisFlags.None);
            context.EnableConcurrentExecution();
            context.RegisterCompilationStartAction(InitializeCompilationAnalysis);
        }

        private void InitializeCompilationAnalysis(CompilationStartAnalysisContext context)
        {
            var analysis = new Analysis(context.Compilation);

            context.RegisterOperationAction(
                analysis.Run,
                OperationKind.ObjectCreation,
                OperationKind.Invocation,
                OperationKind.MethodReference,
                OperationKind.PropertyReference,
                OperationKind.EventReference,
                OperationKind.Throw
            );
        }

        private sealed class Analysis
        {
            private readonly SymbolDisplayFormat _symbolDisplayFormat;
            private readonly INamedTypeSymbol _platformNotSupportedException;
            private readonly INamedTypeSymbol _osPlatformType;
            private readonly IMethodSymbol _isOSPlatformMethod;
            private readonly ConcurrentDictionary<IAssemblySymbol, INamedTypeSymbol> _platformSpecificAttributes = new ConcurrentDictionary<IAssemblySymbol, INamedTypeSymbol>();

            public Analysis(Compilation compilation)
            {
                _symbolDisplayFormat = compilation.Language == LanguageNames.VisualBasic
                        ? SymbolDisplayFormat.VisualBasicShortErrorMessageFormat
                        : SymbolDisplayFormat.CSharpShortErrorMessageFormat;

                _platformNotSupportedException = compilation.GetTypeByMetadataName("System.PlatformNotSupportedException");
                _osPlatformType = compilation.GetTypeByMetadataName("System.Runtime.InteropServices.OSPlatform");
                var runtimeInformationType = compilation.GetTypeByMetadataName("System.Runtime.InteropServices.RuntimeInformation");
                _isOSPlatformMethod = runtimeInformationType?.GetMembers("IsOSPlatform")
                                                             .OfType<IMethodSymbol>()
                                                             .FirstOrDefault(m => m.Parameters.Length == 1 && m.Parameters[0].Type == _osPlatformType);
            }

            public void Run(OperationAnalysisContext context)
            {
                switch (context.Operation.Kind)
                {
                    case OperationKind.ObjectCreation:
                    {
                        var op = (IObjectCreationOperation)context.Operation;
                        AnalyzeSymbolUsage(context, op.Constructor);
                        break;
                    }
                    case OperationKind.Invocation:
                    {
                        var op = (IInvocationOperation)context.Operation;
                        AnalyzeSymbolUsage(context, op.TargetMethod);
                        break;
                    }
                    case OperationKind.MethodReference:
                    case OperationKind.PropertyReference:
                    case OperationKind.EventReference:
                    {
                        var op = (IMemberReferenceOperation)context.Operation;
                        AnalyzeSymbolUsage(context, op.Member);
                        break;
                    }
                    case OperationKind.Throw:
                    {
                        var op = (IThrowOperation)context.Operation;
                        AnalyzeThrowOperation(context, op);
                        break;
                    }

                    default:
                        throw new ArgumentException("Unexpected value for IOperation", nameof(context));
                }
            }

            private void AnalyzeSymbolUsage(OperationAnalysisContext context, ISymbol symbol)
            {
                // Let's be defensive

                if (symbol == null)
                    return;

                // If the symbol isn't marked as platform-specific, this analyzer doesn't have
                // to do any work.

                var referencedLimitations = GetDeclaredPlatforms(symbol);
                if (!referencedLimitations.Any)
                    return;

                // Let's see what the calling method is annotated as platform-specific and wether
                // the calling code is inside a platform-guard.

                var declaredLimitations = GetDeclaredPlatforms(context.ContainingSymbol);
                var guardedPlatforms = GetGuardedPlatforms(context.Operation);

                // If we found guarded platforms, we generally prefer those over any annoations.
                // That makes sense, because the guard will either be more specific or unreachable.

                var callSiteLimitations = guardedPlatforms.Any ? guardedPlatforms : declaredLimitations;

                if (declaredLimitations.Any && guardedPlatforms.Any)
                {
                    // OK, the method we're calling from is platform-specific and a guard is in place.
                    // Let's see whether the guard checks for any platform we're running on. If not,
                    // the code behehind the guard isn't reachable.

                    // TODO: We should mark unreachable guards (via WellKnownDiagnosticTags.Unnecessary).
                    //       If all guards are unreachable, we should mark the entire contents of the if
                    //       block as unreachable.
                    //
                    //       I don't think we'll want to do this here; rather we should do this directly
                    //       in TryGetPlatformGuards(). For that to work, we'd need to pass the declared
                    //       platform limitations.

                    if (!declaredLimitations.ContainsAny(guardedPlatforms))
                        return;
                }

                if (callSiteLimitations.Any)
                {
                    // The call site is platform-specific as is the referenced symbol. That means that
                    // the reference method needs to support all platforms the call site is specific to.

                    callSiteLimitations.RemoveAll(referencedLimitations);

                    if (!callSiteLimitations.Any)
                        return;

                    // The symbol doesn't support all the platforms that the call site is supported on.
                    // Report all missing ones.

                    var formattedSymbol = symbol.ToDisplayString(_symbolDisplayFormat);
                    var formattedLimitations = callSiteLimitations.ToString();
                    var location = context.Operation.Syntax.GetLocation();
                    var diagnostic = Diagnostic.Create(_pc0002, location, formattedSymbol, formattedLimitations);
                    context.ReportDiagnostic(diagnostic);
                }
                else
                {
                    // The call site is supposed to work everywhere but the referenced symbol isn't. Report
                    // a diagnostic.

                    var formattedSymbol = symbol.ToDisplayString(_symbolDisplayFormat);
                    var formattedPlatforms = referencedLimitations.ToString();
                    var location = context.Operation.Syntax.GetLocation();
                    var diagnostic = Diagnostic.Create(_pc0001, location, formattedSymbol, formattedPlatforms);
                    context.ReportDiagnostic(diagnostic);
                }
            }

            private PlatformList GetDeclaredPlatforms(ISymbol symbol)
            {
                TryGetDeclaredPlatforms(symbol, out var platforms);
                return platforms;
            }

            private bool TryGetDeclaredPlatforms(ISymbol symbol, out PlatformList platforms)
            {
                if (TryGetDeclaredPlatformsDirect(symbol, out platforms))
                    return true;

                switch (symbol.Kind)
                {
                    case SymbolKind.NetModule:
                        var module = (IModuleSymbol)symbol;
                        return TryGetDeclaredPlatforms(module.ContainingAssembly, out platforms);
                    case SymbolKind.NamedType:
                        var type = (ITypeSymbol)symbol;
                        if (type.ContainingType != null)
                            return TryGetDeclaredPlatforms(type.ContainingType, out platforms);
                        else
                            return TryGetDeclaredPlatforms(type.ContainingModule, out platforms);
                    case SymbolKind.Method:
                        var method = (IMethodSymbol)symbol;
                        return TryGetDeclaredPlatforms(method.ContainingType, out platforms);
                    case SymbolKind.Property:
                        var property = (IPropertySymbol)symbol;
                        return TryGetDeclaredPlatforms(property.ContainingType, out platforms);
                    case SymbolKind.Event:
                        var @event = (IEventSymbol)symbol;
                        return TryGetDeclaredPlatforms(@event.ContainingType, out platforms);
                }

                return false;
            }

            private bool TryGetDeclaredPlatformsDirect(ISymbol symbol, out PlatformList platforms)
            {
                platforms = new PlatformList();

                var assembly = symbol is IAssemblySymbol a ? a : symbol.ContainingAssembly;
                var platformSpecificAttribute = LookupPlatformSpecificAttribute(assembly);

                foreach (var ca in symbol.GetAttributes())
                {
                    if (ca.AttributeClass == platformSpecificAttribute)
                    {
                        if (ca.ConstructorArguments.Length == 1 &&
                            ca.ConstructorArguments[0].Value is string platform)
                        {
                            platforms.Add(platform);
                        }
                    }
                }

                return platforms.Any;
            }

            private PlatformList GetGuardedPlatforms(IOperation operation)
            {
                var platformGuards = new PlatformList();

                while (operation != null)
                {
                    if (operation is IConditionalOperation conditional)
                    {
                        if (TryGetGuardedPlatformsFromCondition(conditional.Condition, ref platformGuards))
                            break;
                    }

                    operation = operation.Parent;
                }

                return platformGuards;
            }

            private bool TryGetGuardedPlatformsFromCondition(IOperation condition, ref PlatformList platformList)
            {
                switch (condition.Kind)
                {
                    case OperationKind.BinaryOperator:
                    {
                        var binary = (IBinaryOperation)condition;
                        if (binary.OperatorKind == BinaryOperatorKind.Or ||
                            binary.OperatorKind == BinaryOperatorKind.ConditionalOr)
                        {
                            return TryGetGuardedPlatformsFromCondition(binary.LeftOperand, ref platformList) &&
                                   TryGetGuardedPlatformsFromCondition(binary.RightOperand, ref platformList);
                        }
                        break;
                    }

                    case OperationKind.Invocation:
                    {
                        var invocation = (IInvocationOperation) condition;
                        var platform = GetCheckedPlatform(invocation);
                        if (platform != null)
                        {
                            platformList.Add(platform);
                            return true;
                        }
                        break;
                    }
                }

                return false;
            }

            private PlatformList GetRejectedPlatforms(IOperation operation)
            {
                var platformList = new PlatformList();

                while (operation != null)
                {
                    if (operation is IConditionalOperation conditional)
                    {
                        if (TryGetRejectedPlatformsFromCondition(conditional.Condition, ref platformList))
                            break;
                    }

                    operation = operation.Parent;
                }

                return platformList;
            }

            private bool TryGetRejectedPlatformsFromCondition(IOperation condition, ref PlatformList platformList)
            {
                switch (condition.Kind)
                {
                    case OperationKind.BinaryOperator:
                    {
                        var binary = (IBinaryOperation)condition;
                        if (binary.OperatorKind == BinaryOperatorKind.And ||
                            binary.OperatorKind == BinaryOperatorKind.ConditionalAnd)
                        {
                            return TryGetRejectedPlatformsFromCondition(binary.LeftOperand, ref platformList) &&
                                   TryGetRejectedPlatformsFromCondition(binary.RightOperand, ref platformList);
                        }
                        
                        break;
                    }

                    case OperationKind.UnaryOperator:
                    {
                        var unary = (IUnaryOperation)condition;
                        if (unary.OperatorKind == UnaryOperatorKind.Not &&
                            unary.Operand is IInvocationOperation invocation)
                        {
                            var platform = GetCheckedPlatform(invocation);
                            if (platform != null)
                            {
                                platformList.Add(platform);
                                return true;
                            }
                        }

                        break;
                    }
                }

                return false;
            }

            private string GetCheckedPlatform(IInvocationOperation invocation)
            {
                if (invocation.TargetMethod == _isOSPlatformMethod &&
                    invocation.Arguments[0].Value is IPropertyReferenceOperation propertyReference &&
                    propertyReference.Property.Type == _osPlatformType)
                {
                    return propertyReference.Property.Name;
                }

                return null;
            }

            private void AnalyzeThrowOperation(OperationAnalysisContext context, IThrowOperation operation)
            {
                if (operation.Exception.Type != _platformNotSupportedException)
                    return;

                var rejectedPlatforms = GetRejectedPlatforms(operation);

                // If we couldn't find any rejected platforms then the throw operation wasn't
                // inside an if that checked for the absence of specific platforms.

                if (!rejectedPlatforms.Any)
                    return;

                // To check wether we didn't annoate them, we'll subtract the declared platforms
                // from the rejected ones.

                var declaredPlatforms = GetDeclaredPlatforms(context.ContainingSymbol);
                rejectedPlatforms.RemoveAll(declaredPlatforms);

                // If there aren't any left, that means we annoated them all.

                if (!rejectedPlatforms.Any)
                    return;

                // Alright, we missed some. Report them.

                var symbol = context.ContainingSymbol;
                var formattedSymbol = symbol.ToDisplayString(_symbolDisplayFormat);
                var formattedPlatforms = rejectedPlatforms.ToString();
                var location = operation.Syntax.GetLocation();
                var diagnostic = Diagnostic.Create(_pc0003, location, formattedSymbol, formattedPlatforms);
                context.ReportDiagnostic(diagnostic);
            }

            private ITypeSymbol LookupPlatformSpecificAttribute(IAssemblySymbol assembly)
            {
                if (!_platformSpecificAttributes.TryGetValue(assembly, out var result))
                {
                    result = assembly.GetTypeByMetadataName("System.Runtime.CompilerServices.PlatformSpecificAttribute");
                    _platformSpecificAttributes.TryAdd(assembly, result);
                }

                return result;
            }
        }

        private struct PlatformList
        {
            private List<string> _platforms;

            public PlatformList(List<string> platforms)
            {
                _platforms = platforms;
            }

            public bool Any => _platforms != null && _platforms.Count > 0;

            public IEnumerable<string> Items => _platforms == null ? Enumerable.Empty<string>() : _platforms;

            public void Add(string platform)
            {
                if (_platforms == null)
                    _platforms = new List<string>();

                _platforms.Add(platform);
            }

            public void RemoveAll(PlatformList other)
            {
                RemoveAll(other._platforms);
            }

            public void RemoveAll(IEnumerable<string> platforms)
            {
                if (!Any || platforms == null)
                    return;

                foreach (var p in platforms)
                    _platforms.Remove(p);
            }

            public bool ContainsAny(PlatformList other)
            {
                if (!Any || !other.Any)
                    return false;

                foreach (var p in other._platforms)
                    if (_platforms.Contains(p))
                        return true;

                return false;
            }

            public override string ToString()
            {
                if (!Any)
                    return string.Empty;

                // We're sorting because it reads better but also ensures that we have a
                // deterministic text for the diagnostics.
                return string.Join(", ", _platforms.OrderBy(p => p).Select(p => "'" + p + "'"));
            }
        }
    }
}
