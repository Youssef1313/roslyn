// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

using System;
using System.Collections.Immutable;
using System.Threading;
using Microsoft.CodeAnalysis.Diagnostics;
using Microsoft.CodeAnalysis.FlowAnalysis;

namespace Microsoft.CodeAnalysis.Diagnostics
{
    internal interface IAnalysisContext
    {
        public AnalyzerOptions Options { get; }
    }
    internal interface IAnalysisContextWithTree : IAnalysisContext
    {
        public SyntaxTree Tree { get; }
    }

    internal readonly struct IDEAnalysisContext
    {
        public AnalysisContext Context { get; }

        public IDEAnalysisContext(AnalysisContext context)
        {
            Context = context;
        }

        public void RegisterCompilationStartAction(Action<IDECompilationStartAnalysisContext> action) => Context.RegisterCompilationStartAction(context => action(new(context)));

        public void RegisterCompilationAction(Action<IDECompilationAnalysisContext> action) => Context.RegisterCompilationAction(context => action(new(context)));

        public void RegisterSemanticModelAction(Action<IDESemanticModelAnalysisContext> action) => Context.RegisterSemanticModelAction(context => action(new(context)));

        public void RegisterSymbolAction(Action<IDESymbolAnalysisContext> action, ImmutableArray<SymbolKind> symbolKinds) => Context.RegisterSymbolAction(context => action(new(context)), symbolKinds);

        public void RegisterSymbolStartAction(Action<IDESymbolStartAnalysisContext> action, SymbolKind symbolKind) => Context.RegisterSymbolStartAction(context => action(new(context)), symbolKind);

        public void RegisterCodeBlockStartAction<TLanguageKindEnum>(Action<IDECodeBlockStartAnalysisContext<TLanguageKindEnum>> action) where TLanguageKindEnum : struct
            => Context.RegisterCodeBlockStartAction<TLanguageKindEnum>(context => action(new(context)));

        public void RegisterCodeBlockAction(Action<IDECodeBlockAnalysisContext> action) => Context.RegisterCodeBlockAction(context => action(new(context)));

        public void RegisterSyntaxTreeAction(Action<IDESyntaxTreeAnalysisContext> action) => Context.RegisterSyntaxTreeAction(context => action(new(context)));

        public void RegisterAdditionalFileAction(Action<IDEAdditionalFileAnalysisContext> action) => Context.RegisterAdditionalFileAction(context => action(new(context)));

        public void RegisterSyntaxNodeAction<TLanguageKindEnum>(Action<IDESyntaxNodeAnalysisContext> action, ImmutableArray<TLanguageKindEnum> syntaxKinds) where TLanguageKindEnum : struct
            => Context.RegisterSyntaxNodeAction(context => action(new(context)), syntaxKinds);

        public void RegisterSyntaxNodeAction<TLanguageKindEnum>(Action<IDESyntaxNodeAnalysisContext> action, params TLanguageKindEnum[] syntaxKinds) where TLanguageKindEnum : struct
            => Context.RegisterSyntaxNodeAction(context => action(new(context)), syntaxKinds);

        public void RegisterOperationBlockStartAction(Action<IDEOperationBlockStartAnalysisContext> action) => Context.RegisterOperationBlockStartAction(context => action(new(context)));

        public void RegisterOperationBlockAction(Action<IDEOperationBlockAnalysisContext> action) => Context.RegisterOperationBlockAction(context => action(new(context)));

        public void RegisterOperationAction(Action<IDEOperationAnalysisContext> action, ImmutableArray<OperationKind> operationKinds) => Context.RegisterOperationAction(context => action(new(context)), operationKinds);

        public void RegisterOperationAction(Action<IDEOperationAnalysisContext> action, params OperationKind[] operationKinds) => Context.RegisterOperationAction(context => action(new(context)), operationKinds);

        public void EnableConcurrentExecution()
            => Context.EnableConcurrentExecution();

        public void ConfigureGeneratedCodeAnalysis(GeneratedCodeAnalysisFlags analysisMode)
            => Context.ConfigureGeneratedCodeAnalysis(analysisMode);
    }

    internal readonly struct IDECompilationStartAnalysisContext
    {
        public CompilationStartAnalysisContext Context { get; }
        public CancellationToken CancellationToken => Context.CancellationToken;
        public AnalyzerOptions Options => Context.Options;
        public Compilation Compilation => Context.Compilation;

        public IDECompilationStartAnalysisContext(CompilationStartAnalysisContext context)
        {
            Context = context;
        }

        public void RegisterCompilationEndAction(Action<IDECompilationAnalysisContext> action) => Context.RegisterCompilationEndAction(context => action(new(context)));

        public void RegisterSemanticModelAction(Action<IDESemanticModelAnalysisContext> action) => Context.RegisterSemanticModelAction(context => action(new(context)));

        public void RegisterSymbolAction(Action<IDESymbolAnalysisContext> action, ImmutableArray<SymbolKind> symbolKinds) => Context.RegisterSymbolAction(context => action(new(context)), symbolKinds);

        public void RegisterSymbolAction(Action<IDESymbolAnalysisContext> action, params SymbolKind[] symbolKinds) => Context.RegisterSymbolAction(context => action(new(context)), symbolKinds);

        public void RegisterSymbolStartAction(Action<IDESymbolStartAnalysisContext> action, SymbolKind symbolKind) => Context.RegisterSymbolStartAction(context => action(new(context)), symbolKind);

        public void RegisterCodeBlockStartAction<TLanguageKindEnum>(Action<IDECodeBlockStartAnalysisContext<TLanguageKindEnum>> action) where TLanguageKindEnum : struct
            => Context.RegisterCodeBlockStartAction<TLanguageKindEnum>(context => action(new(context)));

        public void RegisterCodeBlockAction(Action<IDECodeBlockAnalysisContext> action) => Context.RegisterCodeBlockAction(context => action(new(context)));

        public void RegisterOperationBlockStartAction(Action<IDEOperationBlockStartAnalysisContext> action) => Context.RegisterOperationBlockStartAction(context => action(new(context)));

        public void RegisterOperationBlockAction(Action<IDEOperationBlockAnalysisContext> action) => Context.RegisterOperationBlockAction(context => action(new(context)));

        public void RegisterSyntaxTreeAction(Action<IDESyntaxTreeAnalysisContext> action) => Context.RegisterSyntaxTreeAction(context => action(new(context)));

        public void RegisterAdditionalFileAction(Action<IDEAdditionalFileAnalysisContext> action) => Context.RegisterAdditionalFileAction(context => action(new(context)));

        public void RegisterSyntaxNodeAction<TLanguageKindEnum>(Action<IDESyntaxNodeAnalysisContext> action, ImmutableArray<TLanguageKindEnum> syntaxKinds) where TLanguageKindEnum : struct
            => Context.RegisterSyntaxNodeAction(context => action(new(context)), syntaxKinds);

        public void RegisterSyntaxNodeAction<TLanguageKindEnum>(Action<IDESyntaxNodeAnalysisContext> action, params TLanguageKindEnum[] syntaxKinds) where TLanguageKindEnum : struct
            => Context.RegisterSyntaxNodeAction(context => action(new(context)), syntaxKinds);

        public void RegisterOperationAction(Action<IDEOperationAnalysisContext> action, ImmutableArray<OperationKind> operationKinds) => Context.RegisterOperationAction(context => action(new(context)), operationKinds);

        public void RegisterOperationAction(Action<IDEOperationAnalysisContext> action, params OperationKind[] operationKinds) => Context.RegisterOperationAction(context => action(new(context)), operationKinds);
    }

    internal readonly struct IDECompilationAnalysisContext
    {
        public CompilationAnalysisContext Context { get; }
        public CancellationToken CancellationToken => Context.CancellationToken;

        public IDECompilationAnalysisContext(CompilationAnalysisContext context)
        {
            Context = context;
        }

        public void ReportDiagnostic(Diagnostic? diagnostic)
        {
            if (diagnostic != null)
            {
                Context.ReportDiagnostic(diagnostic);
            }
        }
    }

    internal readonly struct IDESemanticModelAnalysisContext : IAnalysisContextWithTree
    {
        public SemanticModelAnalysisContext Context { get; }
        public AnalyzerOptions Options => Context.Options;
        public SyntaxTree Tree => Context.SemanticModel.SyntaxTree;
        public SemanticModel SemanticModel => Context.SemanticModel;
        public CancellationToken CancellationToken => Context.CancellationToken;

        public IDESemanticModelAnalysisContext(SemanticModelAnalysisContext context)
        {
            Context = context;
        }

        public void ReportDiagnostic(Diagnostic? diagnostic)
        {
            if (diagnostic != null)
            {
                Context.ReportDiagnostic(diagnostic);
            }
        }
    }

    internal readonly struct IDESymbolAnalysisContext
    {
        public SymbolAnalysisContext Context { get; }
        public Compilation Compilation => Context.Compilation;
        public ISymbol Symbol => Context.Symbol;
        public AnalyzerOptions Options => Context.Options;
        public CancellationToken CancellationToken => Context.CancellationToken;

        public IDESymbolAnalysisContext(SymbolAnalysisContext context)
        {
            Context = context;
        }

        public void ReportDiagnostic(Diagnostic? diagnostic)
        {
            if (diagnostic != null)
            {
                Context.ReportDiagnostic(diagnostic);
            }
        }
    }

    internal readonly struct IDESymbolStartAnalysisContext
    {
        public SymbolStartAnalysisContext Context { get; }
        public ISymbol Symbol => Context.Symbol;
        public CancellationToken CancellationToken => Context.CancellationToken;

        public IDESymbolStartAnalysisContext(SymbolStartAnalysisContext context)
        {
            Context = context;
        }

        public void RegisterSymbolEndAction(Action<IDESymbolAnalysisContext> action) => Context.RegisterSymbolEndAction(context => action(new(context)));

        public void RegisterCodeBlockStartAction<TLanguageKindEnum>(Action<IDECodeBlockStartAnalysisContext<TLanguageKindEnum>> action) where TLanguageKindEnum : struct
            => Context.RegisterCodeBlockStartAction<TLanguageKindEnum>(context => action(new(context)));

        public void RegisterCodeBlockAction(Action<IDECodeBlockAnalysisContext> action) => Context.RegisterCodeBlockAction(context => action(new(context)));

        public void RegisterSyntaxNodeAction<TLanguageKindEnum>(Action<IDESyntaxNodeAnalysisContext> action, ImmutableArray<TLanguageKindEnum> syntaxKinds) where TLanguageKindEnum : struct
            => Context.RegisterSyntaxNodeAction(context => action(new(context)), syntaxKinds);

        public void RegisterOperationBlockStartAction(Action<IDEOperationBlockStartAnalysisContext> action) => Context.RegisterOperationBlockStartAction(context => action(new(context)));

        public void RegisterOperationBlockAction(Action<IDEOperationBlockAnalysisContext> action) => Context.RegisterOperationBlockAction(context => action(new(context)));

        public void RegisterOperationAction(Action<IDEOperationAnalysisContext> action, ImmutableArray<OperationKind> operationKinds) => Context.RegisterOperationAction(context => action(new(context)), operationKinds);

        public void RegisterOperationAction(Action<IDEOperationAnalysisContext> action, params OperationKind[] operationKinds) => Context.RegisterOperationAction(context => action(new(context)), operationKinds);
    }

    internal readonly struct IDECodeBlockStartAnalysisContext<TLanguageKindEnum> : IAnalysisContextWithTree where TLanguageKindEnum : struct
    {
        public CodeBlockStartAnalysisContext<TLanguageKindEnum> Context { get; }
        public AnalyzerOptions Options => Context.Options;
        public SyntaxTree Tree => Context.SemanticModel.SyntaxTree;
        public CancellationToken CancellationToken => Context.CancellationToken;

        public IDECodeBlockStartAnalysisContext(CodeBlockStartAnalysisContext<TLanguageKindEnum> context)
        {
            Context = context;
        }

        public void RegisterCodeBlockEndAction(Action<IDECodeBlockAnalysisContext> action) => Context.RegisterCodeBlockEndAction(context => action(new(context)));

        public void RegisterSyntaxNodeAction(Action<IDESyntaxNodeAnalysisContext> action, ImmutableArray<TLanguageKindEnum> syntaxKinds) => Context.RegisterSyntaxNodeAction(context => action(new(context)), syntaxKinds);
    }

    internal readonly struct IDECodeBlockAnalysisContext : IAnalysisContextWithTree
    {
        public CodeBlockAnalysisContext Context { get; }
        public AnalyzerOptions Options => Context.Options;
        public SyntaxTree Tree => Context.CodeBlock.SyntaxTree;
        public SyntaxNode CodeBlock => Context.CodeBlock;
        public SemanticModel SemanticModel => Context.SemanticModel;
        public CancellationToken CancellationToken => Context.CancellationToken;

        public IDECodeBlockAnalysisContext(CodeBlockAnalysisContext context)
        {
            Context = context;
        }

        public void ReportDiagnostic(Diagnostic? diagnostic)
        {
            if (diagnostic is not null)
            {
                Context.ReportDiagnostic(diagnostic);
            }
        }
    }

    internal readonly struct IDEOperationBlockStartAnalysisContext
    {
        public OperationBlockStartAnalysisContext Context { get; }
        public AnalyzerOptions Options => Context.Options;
        public ImmutableArray<IOperation> OperationBlocks => Context.OperationBlocks;
        public CancellationToken CancellationToken => Context.CancellationToken;

        public IDEOperationBlockStartAnalysisContext(OperationBlockStartAnalysisContext context)
        {
            Context = context;
        }

        public void RegisterOperationBlockEndAction(Action<IDEOperationBlockAnalysisContext> action) => Context.RegisterOperationBlockEndAction(context => action(new(context)));

        public void RegisterOperationAction(Action<IDEOperationAnalysisContext> action, ImmutableArray<OperationKind> operationKinds) => Context.RegisterOperationAction(context => action(new(context)), operationKinds);

        public void RegisterOperationAction(Action<IDEOperationAnalysisContext> action, params OperationKind[] operationKinds) => Context.RegisterOperationAction(context => action(new(context)), operationKinds);
    }

    internal readonly struct IDEOperationBlockAnalysisContext
    {
        public OperationBlockAnalysisContext Context { get; }
        public AnalyzerOptions Options => Context.Options;
        public ImmutableArray<IOperation> OperationBlocks => Context.OperationBlocks;
        public ISymbol OwningSymbol => Context.OwningSymbol;
        public CancellationToken CancellationToken => Context.CancellationToken;

        public IDEOperationBlockAnalysisContext(OperationBlockAnalysisContext context)
        {
            Context = context;
        }

        public void ReportDiagnostic(Diagnostic? diagnostic)
        {
            if (diagnostic is not null)
            {
                Context.ReportDiagnostic(diagnostic);
            }
        }

        public ControlFlowGraph GetControlFlowGraph(IOperation operationBlock) => Context.GetControlFlowGraph(operationBlock);
    }

    internal readonly struct IDESyntaxTreeAnalysisContext : IAnalysisContextWithTree
    {
        public SyntaxTreeAnalysisContext Context { get; }
        public AnalyzerOptions Options => Context.Options;
        public SyntaxTree Tree => Context.Tree;
        public CancellationToken CancellationToken => Context.CancellationToken;

        public IDESyntaxTreeAnalysisContext(SyntaxTreeAnalysisContext context)
        {
            Context = context;
        }

        public void ReportDiagnostic(Diagnostic? diagnostic)
        {
            if (diagnostic is not null)
            {
                Context.ReportDiagnostic(diagnostic);
            }
        }
    }

    internal readonly struct IDEAdditionalFileAnalysisContext
    {
        public AdditionalFileAnalysisContext Context { get; }
        public CancellationToken CancellationToken => Context.CancellationToken;

        public IDEAdditionalFileAnalysisContext(AdditionalFileAnalysisContext context)
        {
            Context = context;
        }

        public void ReportDiagnostic(Diagnostic? diagnostic)
        {
            if (diagnostic is not null)
            {
                Context.ReportDiagnostic(diagnostic);
            }
        }
    }

    internal readonly struct IDESyntaxNodeAnalysisContext : IAnalysisContextWithTree
    {
        public SyntaxNodeAnalysisContext Context { get; }
        public SyntaxNode Node => Context.Node;
        public AnalyzerOptions Options => Context.Options;
        public SyntaxTree Tree => Context.Node.SyntaxTree;
        public SemanticModel SemanticModel => Context.SemanticModel;
        public Compilation Compilation => Context.Compilation;
        public CancellationToken CancellationToken => Context.CancellationToken;

        public IDESyntaxNodeAnalysisContext(SyntaxNodeAnalysisContext context)
        {
            Context = context;
        }

        public void ReportDiagnostic(Diagnostic? diagnostic)
        {
            if (diagnostic is not null)
            {
                Context.ReportDiagnostic(diagnostic);
            }
        }
    }

    internal readonly struct IDEOperationAnalysisContext : IAnalysisContextWithTree
    {
        public OperationAnalysisContext Context { get; }
        public IOperation Operation => Context.Operation;
        public Compilation Compilation => Context.Compilation;
        public AnalyzerOptions Options => Context.Options;
        public SyntaxTree Tree => Context.Operation.Syntax.SyntaxTree;
        public ISymbol ContainingSymbol => Context.ContainingSymbol;
        public CancellationToken CancellationToken => Context.CancellationToken;

        public IDEOperationAnalysisContext(OperationAnalysisContext context)
        {
            Context = context;
        }

        public void ReportDiagnostic(Diagnostic? diagnostic)
        {
            if (diagnostic is not null)
            {
                Context.ReportDiagnostic(diagnostic);
            }
        }
    }
}
