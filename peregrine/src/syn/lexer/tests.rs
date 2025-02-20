#![cfg(test)]
use super::*;
use crate::syn;

fn assert_ws<'a, T, I: Iterator<Item = (T, &'a TokenKind)>>(iter: &mut I, ws: Ws) {
    assert_eq!(iter.next().map(|(_, tok)| tok), Some(&TokenKind::Ws(ws)));
}

fn assert_eof<'a, T, I: Iterator<Item = (T, &'a TokenKind)>>(iter: &mut I) {
    assert_eq!(iter.next().map(|(_, tok)| tok), Some(&TokenKind::Eof));
    assert_eq!(iter.next().map(|(_, tok)| tok), None);
}

#[test]
fn scan_unknown() {
    let tokens = syn::tokenize("@@@");
    let mut iter = tokens
        .iter()
        .map(|(id, tok)| (tokens.get_pos(id), tok.kind()));

    assert_eq!(
        iter.next(),
        Some((
            (Position::new(1, 0), Position::new(1, 3)),
            &TokenKind::Unknown("@@@".into()),
        ))
    );

    assert_eof(&mut iter);
}

#[test]
fn scan_kw() {
    let kws = vec![
        ("module", Keyword::Module),
        ("import", Keyword::Import),
        ("export", Keyword::Export),
        ("public", Keyword::Public),
        ("open", Keyword::Open),
        ("hiding", Keyword::Hiding),
        ("renaming", Keyword::Renaming),
        ("struct", Keyword::Struct),
        ("data", Keyword::Data),
        ("class", Keyword::Class),
        ("instance", Keyword::Instance),
        ("deriving", Keyword::Deriving),
        ("where", Keyword::Where),
        ("let", Keyword::Let),
        ("in", Keyword::In),
        ("do", Keyword::Do),
        ("if", Keyword::If),
        ("then", Keyword::Then),
        ("else", Keyword::Else),
        ("function", Keyword::Function),
        ("match", Keyword::Match),
        ("with", Keyword::With),
        ("forall", Keyword::Forall),
        ("exists", Keyword::Exists),
    ];

    for (str, kw) in kws {
        let tokens = syn::tokenize(str);
        let mut iter = tokens
            .iter()
            .map(|(id, tok)| (tokens.get_pos(id), tok.kind()));

        assert_eq!(
            iter.next(),
            Some((
                (Position::new(1, 0), Position::new(1, str.len())),
                &TokenKind::Kw(kw),
            ))
        );

        assert_eof(&mut iter);
    }
}

#[test]
fn scan_ident() {
    let tokens = syn::tokenize("abc");
    let mut iter = tokens
        .iter()
        .map(|(id, tok)| (tokens.get_pos(id), tok.kind()));

    assert_eq!(
        iter.next(),
        Some((
            (Position::new(1, 0), Position::new(1, 3)),
            &TokenKind::Ident("abc".into()),
        ))
    );

    assert_eof(&mut iter);
}

#[test]
fn scan_ident_with_numerals() {
    let tokens = syn::tokenize("abc12");
    let mut iter = tokens
        .iter()
        .map(|(id, tok)| (tokens.get_pos(id), tok.kind()));

    assert_eq!(
        iter.next(),
        Some((
            (Position::new(1, 0), Position::new(1, 5)),
            &TokenKind::Ident("abc12".into()),
        ))
    );

    assert_eof(&mut iter);
}

#[test]
fn scan_strings() {
    let tokens = syn::tokenize(r#""abc""def\"""#);
    let mut iter = tokens
        .iter()
        .map(|(id, tok)| (tokens.get_pos(id), tok.kind()));

    assert_eq!(
        iter.next(),
        Some((
            (Position::new(1, 0), Position::new(1, 5)),
            &TokenKind::ByteString(ByteString::from_quot(Quotation::Double, r#""abc""#)),
        ))
    );

    assert_eq!(
        iter.next(),
        Some((
            (Position::new(1, 5), Position::new(1, 12)),
            &TokenKind::ByteString(ByteString::from_quot(Quotation::Double, r#""def\"""#)),
        ))
    );

    assert_eof(&mut iter);
}

#[test]
fn scan_empty_string() {
    let tokens = syn::tokenize(r#""""#);
    let mut iter = tokens
        .iter()
        .map(|(id, tok)| (tokens.get_pos(id), tok.kind()));

    assert_eq!(
        iter.next(),
        Some((
            (Position::new(1, 0), Position::new(1, 2)),
            &TokenKind::ByteString(ByteString::from_quot(Quotation::Double, r#""""#)),
        ))
    );

    assert_eof(&mut iter);
}

#[test]
fn scan_erroneous_strings() {
    let tokens = syn::tokenize(r#""abc"#);
    let mut iter = tokens
        .iter()
        .map(|(id, tok)| (tokens.get_pos(id), tok.kind()));

    assert_eq!(
        iter.next(),
        Some((
            (Position::new(1, 0), Position::new(1, 4)),
            &TokenKind::Unknown(r#""abc"#.to_string()),
        ))
    );

    assert_eof(&mut iter);
}

#[test]
fn identifiers_dont_start_with_digits() {
    let tokens = syn::tokenize("1abc");
    let mut iter = tokens
        .iter()
        .map(|(id, tok)| (tokens.get_pos(id), tok.kind()));

    assert_eq!(
        iter.next(),
        Some((
            (Position::new(1, 0), Position::new(1, 4)),
            &TokenKind::Unknown("1abc".to_string()),
        ))
    );

    assert_eof(&mut iter);
}

#[test]
fn multiple_tokens() {
    let tokens = syn::tokenize("abc 123");
    let mut iter = tokens
        .iter()
        .map(|(id, tok)| (tokens.get_pos(id), tok.kind()));

    assert_eq!(
        iter.next(),
        Some((
            (Position::new(1, 0), Position::new(1, 3)),
            &TokenKind::Ident("abc".to_string()),
        ))
    );

    assert_ws(&mut iter, Ws::Space { count: 1 });

    assert_eq!(
        iter.next(),
        Some((
            (Position::new(1, 4), Position::new(1, 7)),
            &TokenKind::Numeral("123".to_string()),
        ))
    );

    assert_eof(&mut iter);
}

#[test]
fn scan_operators() {
    let tokens = syn::tokenize(". .. .| ~ && ~()");
    let mut iter = tokens
        .iter()
        .map(|(id, tok)| (tokens.get_pos(id), tok.kind()));

    assert_eq!(
        iter.next(),
        Some((
            (Position::new(1, 0), Position::new(1, 1)),
            &TokenKind::Operator(".".to_string()),
        ))
    );

    assert_ws(&mut iter, Ws::Space { count: 1 });

    assert_eq!(
        iter.next(),
        Some((
            (Position::new(1, 2), Position::new(1, 4)),
            &TokenKind::Operator("..".to_string()),
        ))
    );

    assert_ws(&mut iter, Ws::Space { count: 1 });

    assert_eq!(
        iter.next(),
        Some((
            (Position::new(1, 5), Position::new(1, 7)),
            &TokenKind::Operator(".|".to_string()),
        ))
    );

    assert_ws(&mut iter, Ws::Space { count: 1 });

    assert_eq!(
        iter.next(),
        Some((
            (Position::new(1, 8), Position::new(1, 9)),
            &TokenKind::Operator("~".to_string()),
        ))
    );

    assert_ws(&mut iter, Ws::Space { count: 1 });

    assert_eq!(
        iter.next(),
        Some((
            (Position::new(1, 10), Position::new(1, 12)),
            &TokenKind::Operator("&&".to_string()),
        ))
    );

    assert_ws(&mut iter, Ws::Space { count: 1 });

    assert_eq!(
        iter.next(),
        Some((
            (Position::new(1, 13), Position::new(1, 14)),
            &TokenKind::Operator("~".to_string()),
        ))
    );

    assert_eq!(
        iter.next(),
        Some((
            (Position::new(1, 14), Position::new(1, 15)),
            &TokenKind::Group(Group::Paren(Parity::Opened)),
        ))
    );

    assert_eq!(
        iter.next(),
        Some((
            (Position::new(1, 15), Position::new(1, 16)),
            &TokenKind::Group(Group::Paren(Parity::Closed)),
        ))
    );

    assert_eof(&mut iter);
}

#[test]
fn scan_with_newlines() {
    let tokens = syn::tokenize("abc def\nghi jkl");
    let mut iter = tokens
        .iter()
        .map(|(id, tok)| (tokens.get_pos(id), tok.kind()));

    assert_eq!(
        iter.next(),
        Some((
            (Position::new(1, 0), Position::new(1, 3)),
            &TokenKind::Ident("abc".to_string()),
        ))
    );

    assert_ws(&mut iter, Ws::Space { count: 1 });

    assert_eq!(
        iter.next(),
        Some((
            (Position::new(1, 4), Position::new(1, 7)),
            &TokenKind::Ident("def".to_string()),
        ))
    );

    assert_ws(&mut iter, Ws::Newline { count: 1 });

    assert_eq!(
        iter.next(),
        Some((
            (Position::new(2, 0), Position::new(2, 3)),
            &TokenKind::Ident("ghi".to_string()),
        ))
    );

    assert_ws(&mut iter, Ws::Space { count: 1 });

    assert_eq!(
        iter.next(),
        Some((
            (Position::new(2, 4), Position::new(2, 7)),
            &TokenKind::Ident("jkl".to_string()),
        ))
    );

    assert_eof(&mut iter);
}

#[test]
fn scan_ws() {
    let tokens = syn::tokenize("\na\n b\n  c\n d\ne");
    let mut iter = tokens.iter().map(|(_, tok)| tok.kind());

    assert_eq!(iter.next(), Some(&TokenKind::Ws(Ws::Newline { count: 1 })));
    assert_eq!(iter.next(), Some(&TokenKind::Ident("a".to_string())));
    assert_eq!(iter.next(), Some(&TokenKind::Ws(Ws::Newline { count: 1 })));
    assert_eq!(iter.next(), Some(&TokenKind::Ws(Ws::Space { count: 1 })));
    assert_eq!(iter.next(), Some(&TokenKind::Ident("b".to_string())));
    assert_eq!(iter.next(), Some(&TokenKind::Ws(Ws::Newline { count: 1 })));
    assert_eq!(iter.next(), Some(&TokenKind::Ws(Ws::Space { count: 2 })));
    assert_eq!(iter.next(), Some(&TokenKind::Ident("c".to_string())));
    assert_eq!(iter.next(), Some(&TokenKind::Ws(Ws::Newline { count: 1 })));
    assert_eq!(iter.next(), Some(&TokenKind::Ws(Ws::Space { count: 1 })));
    assert_eq!(iter.next(), Some(&TokenKind::Ident("d".to_string())));
    assert_eq!(iter.next(), Some(&TokenKind::Ws(Ws::Newline { count: 1 })));
    assert_eq!(iter.next(), Some(&TokenKind::Ident("e".to_string())));
    assert_eq!(iter.next(), Some(&TokenKind::Eof));
    assert_eq!(iter.next(), None);
}

#[test]
fn ws_always_emitted() {
    let token_vec = syn::tokenize("a\n  b\n  c\n  \td\n  \t  e");
    let mut iter = token_vec.iter().map(|(_, tok)| tok.kind());

    assert_eq!(iter.next(), Some(&TokenKind::Ident("a".to_string())));
    assert_eq!(iter.next(), Some(&TokenKind::Ws(Ws::Newline { count: 1 })));
    assert_eq!(iter.next(), Some(&TokenKind::Ws(Ws::Space { count: 2 })));
    assert_eq!(iter.next(), Some(&TokenKind::Ident("b".to_string())));
    assert_eq!(iter.next(), Some(&TokenKind::Ws(Ws::Newline { count: 1 })));
    assert_eq!(iter.next(), Some(&TokenKind::Ws(Ws::Space { count: 2 })));
    assert_eq!(iter.next(), Some(&TokenKind::Ident("c".to_string())));
    assert_eq!(iter.next(), Some(&TokenKind::Ws(Ws::Newline { count: 1 })));
    assert_eq!(iter.next(), Some(&TokenKind::Ws(Ws::Space { count: 2 })));
    assert_eq!(iter.next(), Some(&TokenKind::Ws(Ws::Tab { count: 1 })));
    assert_eq!(iter.next(), Some(&TokenKind::Ident("d".to_string())));
    assert_eq!(iter.next(), Some(&TokenKind::Ws(Ws::Newline { count: 1 })));
    assert_eq!(iter.next(), Some(&TokenKind::Ws(Ws::Space { count: 2 })));
    assert_eq!(iter.next(), Some(&TokenKind::Ws(Ws::Tab { count: 1 })));
    assert_eq!(iter.next(), Some(&TokenKind::Ws(Ws::Space { count: 2 })));
    assert_eq!(iter.next(), Some(&TokenKind::Ident("e".to_string())));
    assert_eq!(iter.next(), Some(&TokenKind::Eof));
    assert_eq!(iter.next(), None);
}

#[test]
fn ws_mixing_can_happen() {
    let tokens = syn::tokenize("a\n  b\n\t\tc");
    let mut iter = tokens.iter().map(|(_, tok)| tok.kind());

    assert_eq!(iter.next(), Some(&TokenKind::Ident("a".to_string())));
    assert_eq!(iter.next(), Some(&TokenKind::Ws(Ws::Newline { count: 1 })));
    assert_eq!(iter.next(), Some(&TokenKind::Ws(Ws::Space { count: 2 })));
    assert_eq!(iter.next(), Some(&TokenKind::Ident("b".to_string())));
    assert_eq!(iter.next(), Some(&TokenKind::Ws(Ws::Newline { count: 1 })));
    assert_eq!(iter.next(), Some(&TokenKind::Ws(Ws::Tab { count: 2 })));
    assert_eq!(iter.next(), Some(&TokenKind::Ident("c".to_string())));
    assert_eq!(iter.next(), Some(&TokenKind::Eof));
    assert_eq!(iter.next(), None);
}
