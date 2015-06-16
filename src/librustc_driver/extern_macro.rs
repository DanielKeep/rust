use std::path::PathBuf;
use std::process::{Command, Stdio};

use rustc::plugin::registry::Registry;
use syntax;
use syntax::ast;
use syntax::codemap::Span;
use syntax::ext::base::{ExtCtxt, MacResult};
use syntax::parse;
use syntax::parse::token;
use syntax::print::pprust;
use syntax::ptr::P;
use syntax::util::small_vector::SmallVector;

pub type ExpandArgs<'cx, 'a, 'b> = (&'cx mut ExtCtxt<'a>, Span, &'b [ast::TokenTree]);

#[derive(Clone, Debug)]
pub struct ExternMacro {
    name: String,
    path: PathBuf,
    api: MacroApi,
}

impl ExternMacro {
    pub fn new(name: String, path: String, api: Option<&str>) -> Result<ExternMacro, String> {
        Ok(ExternMacro {
            name: name,
            path: path.into(),
            api: try!(MacroApi::from_str(api)),
        })
    }

    pub fn run<'cx, 'a, 'b>(&self, args: ExpandArgs<'cx, 'a, 'b>) -> Box<MacResult + 'cx> {
        use self::MacroApi::*;
        match self.api {
            Text => self.run_text(args),
        }
    }

    pub fn run_text<'cx, 'a, 'b>(&self, (cx, sp, tts): ExpandArgs<'cx, 'a, 'b>) -> Box<MacResult + 'cx> {
        macro_rules! cx_failure {
            ($e:expr, $kind:ident, $msg_err:ident => $msg_expr:expr; $cleanup:expr) => {
                match $e {
                    Ok(v) => v,
                    Err($msg_err) => {
                        $cleanup;
                        cx.$kind(sp, $msg_expr);
                        return syntax::ext::base::DummyResult::any(sp);
                    }
                }
            };
        }

        macro_rules! try_err {
            ($e:expr, $msg_expr:expr) => {
                cx_failure!($e, span_err, _ignore => $msg_expr; ())
            };
            ($e:expr, $msg_err:ident => $msg_expr:expr) => {
                cx_failure!($e, span_err, $msg_err => $msg_expr; ())
            };
            ($e:expr, $msg_err:ident => $msg_expr:expr; $cleanup:expr) => {
                cx_failure!($e, span_err, $msg_err => $msg_expr; $cleanup)
            };
        }

        macro_rules! err {
            ($msg:expr) => {
                {
                    cx_failure!(Err::<(), ()>(()), span_err, _ignore => $msg; ());
                    unreachable!()
                }
            };
        }

        let span_str = {
            use syntax::codemap::Pos;
            format!("{},{},{}", sp.lo.to_usize(), sp.hi.to_usize(), sp.expn_id.into_u32())
        };

        let emp = Command::new(&self.path)
            .arg(&self.name)
            .env("RUST_MACRO_NAME", &self.name)
            .env("RUST_MACRO_API", self.api.to_str())
            .env("RUST_VERSION", option_env!("CFG_VERSION").unwrap_or("unknown"))
            .env("RUST_SPAN", &span_str)
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .spawn();

        let mut emp = try_err!(emp,
            err => &format!("failure while invoking external macro `{}`: {:?}", self.name, err));

        let mut stdin = try_err!(emp.stdin.take().ok_or(()),
            &format!("could not get input pipe for external macro `{}`", self.name));

        // Give the macro the invocation arguments.
        {
            use std::io::Write;

            let src = pprust::tts_to_string(tts);
            try_err!(writeln!(stdin, "{}", src).and_then(|_| stdin.flush()),
                err => &format!("error writing to external macro input: {:?}", err);
                { let _ = emp.kill(); });

            // Drop stdin in order to close it to help avoid deadlocks.
            drop(stdin);
        }

        // // Read back the output.
        let exp_src = match emp.wait_with_output() {
            Ok(out) => {
                if out.status.success() {
                    try_err!(String::from_utf8(out.stdout),
                        err => &format!("error decoding external macro output: {:?}", err))
                } else {
                    err!(&format!("external macro failed with status {}",
                        out.status.code().unwrap_or(0)))
                }
            },
            Err(err) => err!(&format!("external macro failed: {:?}", err))
        };

        // Parse the output and construct the result.
        let p = parse::new_parser_from_source_str(
            cx.parse_sess(),
            cx.cfg(),
            format!("<{} expansion>", self.name),
            exp_src);

        Box::new(ExpandResult { p: p })
    }
}

#[derive(Clone, Copy, Debug)]
pub enum MacroApi {
    Text,
}

impl MacroApi {
    fn from_str(s: Option<&str>) -> Result<MacroApi, String> {
        use self::MacroApi::*;
        match s {
            None => Ok(Text),
            Some(s) => match s {
                "text" => Ok(Text),
                _ => Err(format!("unknown extern macro api `{}`", s))
            }
        }
    }

    fn to_str(&self) -> &'static str {
        use self::MacroApi::*;
        match *self {
            Text => "text",
        }
    }
}

pub fn tts_to_json(tts: &[ast::TokenTree]) -> serialize::json::Json {
    use std::collections::BTreeMap;
    use serialize::json::{self, Json};
    use syntax::ast::TokenTree::*;

    macro_rules! jo {
        ($($k:expr, $v:expr),* $(,)*) => (
            {
                let mut map = BTreeMap::new();
                $(map.insert($k.into(), $v.into());)*
                Json::Object(map)
            }
        );
    }

    fn sp_to_json(sp: &Span) -> Json {
        Json::String(format!("{},{},{}",
            sp.lo.to_usize(), sp.hi.to_usize(), sp.expn_id.into_u32()))
    }

    fn tok_to_json(tok: &token::Token) -> Json {
        use self::token::Token::*;

        macro_rules! lit_tok {
            ($s:expr) => (Json::String($s.into()));
        }

        match *tok {
            Eq => lit_tok!("="),
            Lt => lit_tok!("<"),
            Le => lit_tok!("<="),
            EqEq => lit_tok!("=="),
            Ne => lit_tok!("!="),
            Ge => lit_tok!(">="),
            Gt => lit_tok!(">"),
            LASTEDIT
        }
    }

    let mut arr = vec![];
    for tt in tts {
        match *tt {
            TtToken(ref sp, ref tok) => {}
        }
    }
}

pub fn register_extern_macro(registry: &mut Registry, em: ExternMacro) {
    let name = em.name.clone();
    let expander = ExternExpander { em: em };
    registry.register_syntax_extension(
        token::intern(&*name),
        syntax::ext::base::NormalTT(Box::new(expander), None, false));
}

// Why didn't I just use a closure?  Because I could not, for the life of me, work out how to get
// the appropriate lifetimes.  There is no other reason for this struct.
// -- DanielKeep

struct ExternExpander {
    em: ExternMacro,
}

impl<'cx, 'a, 'b> Fn<ExpandArgs<'cx, 'a, 'b>> for ExternExpander {
    extern "rust-call" fn call(&self, args: ExpandArgs<'cx, 'a, 'b>) -> Self::Output {
        self.em.run(args)
    }
}

impl<'cx, 'a, 'b> FnMut<ExpandArgs<'cx, 'a, 'b>> for ExternExpander {
    extern "rust-call" fn call_mut(&mut self, args: ExpandArgs<'cx, 'a, 'b>) -> Self::Output {
        self.call(args)
    }
}

impl<'cx, 'a, 'b> FnOnce<ExpandArgs<'cx, 'a, 'b>> for ExternExpander {
    type Output = Box<MacResult + 'cx>;

    extern "rust-call" fn call_once(self, args: ExpandArgs<'cx, 'a, 'b>) -> Self::Output {
        self.call(args)
    }
}

// This is stolen from syntax::ext::source_util::expand_include.

struct ExpandResult<'a> {
    p: parse::parser::Parser<'a>,
}

impl<'a> MacResult for ExpandResult<'a> {
    fn make_expr(mut self: Box<ExpandResult<'a>>) -> Option<P<ast::Expr>> {
        Some(self.p.parse_expr())
    }
    fn make_items(mut self: Box<ExpandResult<'a>>)
                  -> Option<SmallVector<P<ast::Item>>> {
        let mut ret = SmallVector::zero();
        while self.p.token != token::Eof {
            match self.p.parse_item() {
                Some(item) => ret.push(item),
                None => panic!(self.p.span_fatal(
                    self.p.span,
                    &format!("expected item, found `{}`",
                             self.p.this_token_to_string())
                ))
            }
        }
        Some(ret)
    }
}
