/* MIT License

Copyright (c) 2023 National University of Singapore

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE. */

use lang_c::ast::TranslationUnit;
use lang_c::driver::{Config, parse as parse_c, Error as ParseError};

use clap::Parser as ClapParser;

mod lang_defs;
mod lang;
mod utils;
mod dag;
mod dag_builder;
mod codegen;

use lang::CaplanTranslationUnit;
use codegen::CodeGen;

#[derive(ClapParser)]
#[clap(author, version, about)]
struct Args {
    source: String,
    #[clap(long, default_value_t = 500)]
    clock_rate: u32,
    #[clap(long, default_value_t = 1<<14)]
    stack_size: u32,
    #[clap(long, default_value_t = 1<<16)]
    mem_size: usize,
    #[clap(long, default_value_t = 32)]
    gpr_n: usize,
    #[clap(long)]
    show_ast: bool,
    #[clap(long)]
    dag_dot: bool,
    #[clap(long)]
    print_comments: bool
}


fn generate_code(translation_unit: &TranslationUnit) {
    let codegen = CodeGen::new(CaplanTranslationUnit::from_ast(translation_unit));
    codegen.codegen()
}

fn main() {
    let cli_args = Args::parse();
    let config = Config::default();
    match parse_c(&config, &cli_args.source) {
        Ok(parser_result)  => {
            if cli_args.show_ast {
                eprintln!("AST:\n{:#?}", &parser_result);
            } 
            generate_code(&parser_result.unit);
        }
        Err(e) => {
            eprintln!("Parse error!");
            match e {
                ParseError::SyntaxError(syntax_error) => {
                    eprintln!("Source code:");
                    for (idx, line) in syntax_error.source.lines().enumerate() {
                        eprintln!("{:4}  {}", idx + 1, line);
                    }
                    eprintln!("At line {}, column {}", syntax_error.line, syntax_error.column);
                }
                _ => {
                    eprintln!("{:#?}", e);
                }
            }
        }
    }
}

