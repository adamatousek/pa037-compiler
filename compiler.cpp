SEAGOL_RELAX_WARNINGS
#include <llvm/Support/raw_os_ostream.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/Pass.h>
#include <llvm/IR/Verifier.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/Support/FileSystem.h>
#include <llvm/Support/TargetRegistry.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Target/TargetOptions.h>
SEAGOL_UNRELAX_WARNINGS

#include "compiler.hpp"

namespace seagol {

bool Compiler::prepare_target()
{
    target_triple = llvm::sys::getDefaultTargetTriple();
    llvm::InitializeAllTargetInfos();
    llvm::InitializeAllTargets();
    llvm::InitializeAllTargetMCs();
    llvm::InitializeAllAsmParsers();
    llvm::InitializeAllAsmPrinters();

    std::string err;
    auto target = llvm::TargetRegistry::lookupTarget( target_triple, err );
    if ( !target ) {
        errf << err << std::endl;
        return false;
    }

    std::string cpu = "generic";
    std::string features = "";
    llvm::TargetOptions target_opts;
    auto reloc_model = llvm::Optional<llvm::Reloc::Model>();
    target_machine = target->createTargetMachine( target_triple, cpu,
            features, target_opts, reloc_model );

    ctx.llmodule->setDataLayout( target_machine->createDataLayout() );
    ctx.llmodule->setTargetTriple( target_triple );
    return true;
}

int Compiler::run()
{
    if ( !prepare_target() )
        return 1;

    auto putn = [this]( int n, char c ){
        for ( int i = 0; i < n; ++i )
            errf << c;
    };

    int parse_error = 1;
    try {
        parse_error = parser.parse();
    } catch ( yy::parser::syntax_error &e ) {
        if ( e.location == lexer.dummy_location ) {
            errf << e.what() << std::endl;
        } else {
            errf << e.location << ": " << e.what() << std::endl;
            lexer.readRestOfLine();
            errf << lexer.last_line << std::endl;
            auto from = e.location.begin.column - 1;
            putn( from, ' ' );
            putn( e.location.end.column - from - 1, '^' );
            errf << std::endl;
        }
    }
    if ( parse_error ) {
        errf << "seagolc: compilation failed." << std::endl;
        return 1;
    }

    errf << "seagolc: compilation successful\nLLVM bitcode:\n" << std::endl;

    ctx.llmodule->print(llvm::outs(), nullptr);

    llvm::ModuleAnalysisManager am;
    llvm::VerifierAnalysis ver;
    ver.run( *ctx.llmodule, am );

    // llvm::raw_os_ostream lloutf{ outf };
    // ctx.llmodule->print(lloutf, nullptr);

    std::error_code errc;
    llvm::raw_fd_ostream dest("out.o", errc, llvm::sys::fs::OF_None);
    if ( errc ) {
        errf << "cannot open file for writing: " << errc.message() << std::endl;
        return 1;
    }

    llvm::legacy::PassManager pm;
    auto ft_obj = llvm::TargetMachine::CGFT_ObjectFile;
    if ( target_machine->addPassesToEmitFile( pm, dest, nullptr, ft_obj ) ) {
        errf << "unable to emit object file" << std::endl;
        return 1;
    }

    pm.run( *ctx.llmodule );
    dest.flush();

    return 0;
}

void Compiler::setLocation( const std::string &file, unsigned line,
                            unsigned column)
{
    auto &l = lexer.loc;
    delete l.begin.filename;
    delete l.end.filename;
    l.end.filename = new std::string( file );
    l.end.line = line;
    l.end.column = column;
    l.step();
}

} /* seagol */

namespace yy {
/* Flex-Bison Glue */
parser::symbol_type yylex( seagol::Lexer &lexer ) { return lexer.next(); }

/* Bison++ needs this defined */
void parser::error( const location_type &l, const std::string &m ) {
    throw syntax_error( l, m );
}
}

