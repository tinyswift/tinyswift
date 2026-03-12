//===--- ClangImporterStubs.cpp - Stubs for TinySwift build ---------------===//
//
// Provides stub implementations for ClangImporter methods when building
// TinySwift without the full ClangImporter library.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/ASTContext.h"
#include "swift/AST/Decl.h"
#include "swift/AST/DeclContext.h"
#include "swift/AST/Evaluator.h"
#include "swift/AST/Module.h"
#include "swift/AST/ModuleDependencies.h"
#include "swift/AST/SourceFile.h"
#include "swift/AST/Type.h"
#include "swift/ClangImporter/ClangImporter.h"
#include "swift/ClangImporter/ClangImporterRequests.h"
#include "swift/ClangImporter/ClangModule.h"
#include "swift/IDE/SourceEntityWalker.h"
#include "swift/Subsystems.h"
#include "llvm/Support/ErrorHandling.h"

// Include the internal header for EffectiveClangContext
#include "SwiftLookupTable.h"

using namespace swift;

// ============================================================================
// ClangImporter static methods
// ============================================================================

std::unique_ptr<ClangImporter>
ClangImporter::create(ASTContext &ctx, std::string swiftPCHHash,
                      DependencyTracker *tracker,
                      DWARFImporterDelegate *dwarfImporterDelegate,
                      bool ignoreFileMapping) {
  llvm_unreachable("ClangImporter not available in TinySwift");
}

std::shared_ptr<clang::DependencyCollector>
ClangImporter::createDependencyCollector(
    IntermoduleDepTrackingMode Mode,
    std::shared_ptr<llvm::FileCollectorBase> FileCollector) {
  return nullptr;
}

// ============================================================================
// ClangImporter instance methods
// ============================================================================

ClangImporter::~ClangImporter() {}

void ClangImporter::collectVisibleTopLevelModuleNames(
    SmallVectorImpl<Identifier> &names) const {}

bool ClangImporter::canImportModule(ImportPath::Module named, SourceLoc loc,
                                    ModuleVersionInfo *versionInfo,
                                    bool isTestableImport) {
  return false;
}

ModuleDecl *ClangImporter::loadModule(SourceLoc importLoc,
                                      ImportPath::Module path,
                                      bool AllowMemoryCache) {
  return nullptr;
}

bool ClangImporter::isInOverlayModuleForImportedModule(
    const DeclContext *overlayDC, const DeclContext *importedDC) {
  return false;
}

void ClangImporter::lookupValue(DeclName name, VisibleDeclConsumer &consumer) {}

void ClangImporter::lookupTypeDecl(
    StringRef clangName, ClangTypeKind kind,
    llvm::function_ref<void(TypeDecl *)> receiver) {}

void ClangImporter::lookupRelatedEntity(
    StringRef clangName, ClangTypeKind kind, StringRef relatedEntityKind,
    llvm::function_ref<void(TypeDecl *)> receiver) {}

StructDecl *ClangImporter::instantiateCXXClassTemplate(
    clang::ClassTemplateDecl *decl,
    ArrayRef<clang::TemplateArgument> arguments) {
  return nullptr;
}

ConcreteDeclRef
ClangImporter::getCXXFunctionTemplateSpecialization(SubstitutionMap subst,
                                                    ValueDecl *decl) {
  return ConcreteDeclRef();
}

void ClangImporter::loadExtensions(NominalTypeDecl *nominal,
                                   unsigned previousGeneration) {}

void ClangImporter::loadObjCMethods(
    NominalTypeDecl *typeDecl, ObjCSelector selector, bool isInstanceMethod,
    unsigned previousGeneration,
    llvm::TinyPtrVector<AbstractFunctionDecl *> &methods) {}

bool ClangImporter::addSearchPath(StringRef newSearchPath, bool isFramework,
                                  bool isSystem) {
  return false;
}

bool ClangImporter::importBridgingHeader(StringRef header, ModuleDecl *adapter,
                                         SourceLoc diagLoc,
                                         bool trackParsedSymbols,
                                         bool implicitImport) {
  return true; // error
}

ModuleDecl *ClangImporter::getImportedHeaderModule() const {
  return nullptr;
}

ModuleDecl *ClangImporter::getWrapperForModule(const clang::Module *mod,
                                               bool returnOverlayIfPossible) const {
  return nullptr;
}

void ClangImporter::verifyAllModules() {}

llvm::SmallVector<std::pair<ModuleDependencyID, ModuleDependencyInfo>, 1>
ClangImporter::getModuleDependencies(
    Identifier moduleName, StringRef moduleOutputPath,
    const llvm::DenseSet<clang::tooling::dependencies::ModuleID>
        &alreadySeenClangModules,
    clang::tooling::dependencies::DependencyScanningTool &clangScanningTool,
    InterfaceSubContextDelegate &delegate, llvm::PrefixMapper *mapper,
    bool isTestableImport) {
  return {};
}

bool ClangImporter::getHeaderDependencies(
    ModuleDependencyID moduleID,
    clang::tooling::dependencies::DependencyScanningTool &clangScanningTool,
    ModuleDependenciesCache &cache,
    ModuleDependencyIDSetVector &headerClangModuleDependencies,
    std::vector<std::string> &headerFileInputs,
    std::vector<std::string> &bridgingHeaderCommandLine,
    std::optional<std::string> &includeTreeID) {
  return true; // error
}

void ClangImporter::addClangInvovcationDependencies(
    std::vector<std::string> &files) {}

DeclName ClangImporter::importName(const clang::NamedDecl *D,
                                   clang::DeclarationName givenName) {
  return DeclName();
}

std::optional<Type>
ClangImporter::importFunctionReturnType(const clang::FunctionDecl *clangDecl,
                                        DeclContext *dc) {
  return std::nullopt;
}

Type ClangImporter::importVarDeclType(const clang::VarDecl *clangDecl,
                                      VarDecl *swiftDecl, DeclContext *dc) {
  return Type();
}

bool ClangImporter::isUnsafeCXXMethod(const FuncDecl *func) { return false; }

Decl *ClangImporter::importDeclDirectly(const clang::NamedDecl *decl) {
  return nullptr;
}

void ClangImporter::diagnoseMemberValue(const DeclName &name,
                                        const Type &baseType) {}

bool ClangImporter::isCXXMethodMutating(const clang::CXXMethodDecl *method) {
  return false;
}

void ClangImporter::diagnoseTopLevelValue(const DeclName &name) {}

ValueDecl *ClangImporter::importBaseMemberDecl(ValueDecl *decl,
                                               DeclContext *newContext) {
  return nullptr;
}

SourceLoc ClangImporter::importSourceLocation(clang::SourceLocation loc) {
  return SourceLoc();
}

clang::FunctionDecl *ClangImporter::instantiateCXXFunctionTemplate(
    ASTContext &ctx, clang::FunctionTemplateDecl *func,
    SubstitutionMap subst) {
  return nullptr;
}

const clang::TypedefType *
ClangImporter::getTypeDefForCXXCFOptionsDefinition(
    const clang::Decl *candidateDecl) {
  return nullptr;
}

FuncDecl *ClangImporter::getDefaultArgGenerator(
    const clang::ParmVarDecl *param) {
  return nullptr;
}

SwiftLookupTable *
ClangImporter::findLookupTable(const clang::Module *clangModule) {
  return nullptr;
}

EffectiveClangContext
ClangImporter::getEffectiveClangContext(const NominalTypeDecl *nominal) {
  return EffectiveClangContext();
}

std::vector<std::string>
ClangImporter::getSwiftExplicitModuleDirectCC1Args() const {
  return {};
}

// ============================================================================
// ClangImporter const methods
// ============================================================================

clang::TargetInfo &ClangImporter::getTargetInfo() const {
  llvm_unreachable("ClangImporter not available in TinySwift");
}

clang::CodeGenOptions &ClangImporter::getCodeGenOpts() const {
  llvm_unreachable("ClangImporter not available in TinySwift");
}

clang::TargetInfo &ClangImporter::getModuleAvailabilityTarget() const {
  llvm_unreachable("ClangImporter not available in TinySwift");
}

clang::ASTContext &ClangImporter::getClangASTContext() const {
  llvm_unreachable("ClangImporter not available in TinySwift");
}

clang::Preprocessor &ClangImporter::getClangPreprocessor() const {
  llvm_unreachable("ClangImporter not available in TinySwift");
}

clang::Sema &ClangImporter::getClangSema() const {
  llvm_unreachable("ClangImporter not available in TinySwift");
}

const clang::CompilerInstance &ClangImporter::getClangInstance() const {
  llvm_unreachable("ClangImporter not available in TinySwift");
}

const clang::Type *ClangImporter::parseClangFunctionType(StringRef type,
                                                         SourceLoc loc) const {
  return nullptr;
}

void ClangImporter::printClangType(const clang::Type *type,
                                   llvm::raw_ostream &os) const {}

StableSerializationPath
ClangImporter::findStableSerializationPath(const clang::Decl *decl) const {
  return StableSerializationPath();
}

const clang::Decl *ClangImporter::resolveStableSerializationPath(
    const StableSerializationPath &path) const {
  return nullptr;
}

bool ClangImporter::isSerializable(const clang::Type *type,
                                   bool checkCanonical) const {
  return false;
}

void ClangImporter::printStatistics() const {}
void ClangImporter::dumpSwiftLookupTables() const {}

// ============================================================================
// ClangModuleUnit stubs
// ============================================================================

ModuleDecl *ClangModuleUnit::getOverlayModule() const {
  return nullptr;
}

StringRef ClangModuleUnit::getExportedModuleName() const {
  return StringRef();
}

bool ClangModuleUnit::isSystemModule() const {
  return false;
}

void ClangModuleUnit::lookupValue(DeclName name, NLKind lookupKind,
                                  OptionSet<ModuleLookupFlags> Flags,
                                  SmallVectorImpl<ValueDecl *> &results) const {}

TypeDecl *ClangModuleUnit::lookupNestedType(Identifier name,
                                            const NominalTypeDecl *baseType) const {
  return nullptr;
}

void ClangModuleUnit::lookupVisibleDecls(ImportPath::Access accessPath,
                                         VisibleDeclConsumer &consumer,
                                         NLKind lookupKind) const {}

void ClangModuleUnit::lookupClassMembers(ImportPath::Access accessPath,
                                         VisibleDeclConsumer &consumer) const {}

void ClangModuleUnit::lookupClassMember(ImportPath::Access accessPath,
                                        DeclName name,
                                        SmallVectorImpl<ValueDecl *> &decls) const {}

void ClangModuleUnit::lookupObjCMethods(
    ObjCSelector selector,
    SmallVectorImpl<AbstractFunctionDecl *> &results) const {}

bool ClangModuleUnit::shouldCollectDisplayDecls() const {
  return false;
}

void ClangModuleUnit::getTopLevelDecls(SmallVectorImpl<Decl *> &results) const {}

void ClangModuleUnit::getDisplayDecls(SmallVectorImpl<Decl *> &results,
                                      bool recursive) const {}

void ClangModuleUnit::getImportedModules(
    SmallVectorImpl<ImportedModule> &imports,
    ModuleDecl::ImportFilter filter) const {}

void ClangModuleUnit::getImportedModulesForLookup(
    SmallVectorImpl<ImportedModule> &imports) const {}

void ClangModuleUnit::collectLinkLibraries(
    ModuleDecl::LinkLibraryCallback callback) const {}

StringRef ClangModuleUnit::getFilename() const {
  return StringRef();
}

StringRef ClangModuleUnit::getLoadedFilename() const {
  return StringRef();
}

StringRef ClangModuleUnit::getModuleDefiningPath() const {
  return StringRef();
}

// ============================================================================
// ObjC implementation methods (on AST classes, defined in ClangImporter)
// ============================================================================

IterableDeclContext *IterableDeclContext::getImplementationContext() {
  return this;
}

DeclContext *DeclContext::getImplementedObjCContext() const {
  return const_cast<DeclContext *>(this);
}

Decl *Decl::getObjCImplementationDecl() const {
  return nullptr;
}

llvm::TinyPtrVector<Decl *> Decl::getAllImplementedObjCDecls() const {
  return {};
}

// ============================================================================
// simple_display / extractNearestSourceLoc for ClangImporter request types
// ============================================================================

void swift::simple_display(llvm::raw_ostream &out,
                           CxxRecordSemanticsDescriptor desc) {
  out << "(CxxRecordSemanticsDescriptor)";
}

SourceLoc swift::extractNearestSourceLoc(CxxRecordSemanticsDescriptor desc) {
  return SourceLoc();
}

void swift::simple_display(llvm::raw_ostream &out,
                           const ObjCInterfaceAndImplementation &pair) {
  out << "(ObjCInterfaceAndImplementation)";
}

SourceLoc swift::extractNearestSourceLoc(
    const ObjCInterfaceAndImplementation &pair) {
  return SourceLoc();
}

void swift::simple_display(llvm::raw_ostream &out,
                           const ClangCategoryLookupDescriptor &desc) {
  out << "(ClangCategoryLookupDescriptor)";
}

SourceLoc swift::extractNearestSourceLoc(
    const ClangCategoryLookupDescriptor &desc) {
  return SourceLoc();
}

void swift::simple_display(llvm::raw_ostream &out,
                           SafeUseOfCxxDeclDescriptor desc) {
  out << "(SafeUseOfCxxDeclDescriptor)";
}

SourceLoc swift::extractNearestSourceLoc(SafeUseOfCxxDeclDescriptor desc) {
  return SourceLoc();
}

void swift::simple_display(llvm::raw_ostream &out,
                           EscapabilityLookupDescriptor desc) {
  out << "(EscapabilityLookupDescriptor)";
}

SourceLoc swift::extractNearestSourceLoc(EscapabilityLookupDescriptor desc) {
  return SourceLoc();
}

// ============================================================================
// ClangImporter request evaluate() stubs
// ============================================================================

CxxRecordSemanticsKind CxxRecordSemantics::evaluate(
    Evaluator &evaluator, CxxRecordSemanticsDescriptor desc) const {
  return CxxRecordSemanticsKind::MissingLifetimeOperation;
}

CustomRefCountingOperationResult CustomRefCountingOperation::evaluate(
    Evaluator &evaluator, CustomRefCountingOperationDescriptor desc) const {
  return {};
}

SmallVector<SingleEntry, 4> ClangDirectLookupRequest::evaluate(
    Evaluator &evaluator, ClangDirectLookupDescriptor desc) const {
  return {};
}

ObjCInterfaceAndImplementation
ObjCInterfaceAndImplementationRequest::evaluate(Evaluator &evaluator,
                                                Decl *decl) const {
  return {};
}

std::optional<ObjCInterfaceAndImplementation>
ObjCInterfaceAndImplementationRequest::getCachedResult() const {
  return ObjCInterfaceAndImplementation{};
}

void ObjCInterfaceAndImplementationRequest::cacheResult(
    ObjCInterfaceAndImplementation value) const {}

llvm::TinyPtrVector<Decl *> ClangCategoryLookupRequest::evaluate(
    Evaluator &evaluator, ClangCategoryLookupDescriptor desc) const {
  return {};
}

bool IsSafeUseOfCxxDecl::evaluate(Evaluator &evaluator,
                                  SafeUseOfCxxDeclDescriptor desc) const {
  return true;
}

ValueDecl *CxxRecordAsSwiftType::evaluate(Evaluator &evaluator,
                                          CxxRecordSemanticsDescriptor desc) const {
  return nullptr;
}

CxxEscapability ClangTypeEscapability::evaluate(
    Evaluator &evaluator, EscapabilityLookupDescriptor desc) const {
  return CxxEscapability::Unknown;
}

TinyPtrVector<ValueDecl *> ClangRecordMemberLookup::evaluate(
    Evaluator &evaluator, ClangRecordMemberLookupDescriptor desc) const {
  return {};
}

TinyPtrVector<ValueDecl *> CXXNamespaceMemberLookup::evaluate(
    Evaluator &evaluator, CXXNamespaceMemberLookupDescriptor desc) const {
  return {};
}

// Register request functions
static AbstractRequestFunction *clangImporterRequestFunctions[] = {
#define SWIFT_REQUEST(Zone, Name, Sig, Caching, LocOptions)                    \
  reinterpret_cast<AbstractRequestFunction *>(&Name::evaluateRequest),
#include "swift/ClangImporter/ClangImporterTypeIDZone.def"
#undef SWIFT_REQUEST
};

void swift::registerClangImporterRequestFunctions(Evaluator &evaluator) {
  evaluator.registerRequestFunctions(Zone::ClangImporter,
                                     clangImporterRequestFunctions);
}

// ============================================================================
// SourceEntityWalker stubs (defined in lib/IDE/)
// ============================================================================

bool SourceEntityWalker::walk(SourceFile &SrcFile) { return true; }

bool SourceEntityWalker::visitCallArgName(Identifier Name,
                                          CharSourceRange Range,
                                          ValueDecl *D) {
  return true;
}

bool SourceEntityWalker::visitModuleReference(ModuleEntity Mod,
                                              CharSourceRange Range) {
  return true;
}

bool SourceEntityWalker::visitSubscriptReference(ValueDecl *D,
                                                 CharSourceRange Range,
                                                 ReferenceMetaData Data,
                                                 bool IsOpenBracket) {
  return true;
}

bool SourceEntityWalker::visitCallAsFunctionReference(ValueDecl *D,
                                                      CharSourceRange Range,
                                                      ReferenceMetaData Data) {
  return true;
}

bool SourceEntityWalker::visitDeclarationArgumentName(Identifier Name,
                                                      SourceLoc StartLoc,
                                                      ValueDecl *D) {
  return true;
}

void SourceEntityWalker::anchor() {}

// ============================================================================
// DWARFImporterDelegate stubs
// ============================================================================

void DWARFImporterDelegate::anchor() {}

// ============================================================================
// importer:: namespace stubs
// ============================================================================

namespace swift {
namespace importer {

bool isCxxStdModule(const clang::Module *module) { return false; }
bool isCxxStdModule(StringRef moduleName, bool IsSystem) { return false; }

bool isCxxConstReferenceType(const clang::Type *type) { return false; }

ValueDecl *getImportedMemberOperator(const DeclBaseName &name,
                                     NominalTypeDecl *selfType,
                                     std::optional<Type> parameterType) {
  return nullptr;
}

} // namespace importer

// ============================================================================
// Utility function stubs
// ============================================================================

std::string
getModuleCachePathFromClang(const clang::CompilerInstance &Instance) {
  return "";
}

} // namespace swift
