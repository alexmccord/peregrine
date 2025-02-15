#![cfg(test)]
use super::*;

fn skip_ws(lexer: &mut Lexer, ws: tok::Ws) {
    let tok = lexer.next().unwrap();
    assert_eq!(lexer[tok], tok::TokenKind::Ws(ws));
}

#[test]
fn scan_unknown() {
    let mut lexer = Lexer::new("@@@");

    let tok1 = lexer.next().unwrap();
    assert_eq!(lexer[tok1], tok::TokenKind::Unknown("@@@".into()));

    let tok2 = lexer.next().unwrap();
    assert_eq!(lexer[tok2], tok::TokenKind::Eof);

    assert_eq!(lexer.next(), None);

    let (begin, end) = lexer.tokens.get_pos(tok1);
    assert_eq!(begin, Position::new(1, 0));
    assert_eq!(end, Position::new(1, 3));
}

#[test]
fn scan_kw() {
    let kws = vec![
        ("module", tok::Keyword::Module),
        ("import", tok::Keyword::Import),
        ("export", tok::Keyword::Export),
        ("public", tok::Keyword::Public),
        ("open", tok::Keyword::Open),
        ("hiding", tok::Keyword::Hiding),
        ("renaming", tok::Keyword::Renaming),
        ("struct", tok::Keyword::Struct),
        ("data", tok::Keyword::Data),
        ("class", tok::Keyword::Class),
        ("instance", tok::Keyword::Instance),
        ("deriving", tok::Keyword::Deriving),
        ("where", tok::Keyword::Where),
        ("let", tok::Keyword::Let),
        ("in", tok::Keyword::In),
        ("do", tok::Keyword::Do),
        ("if", tok::Keyword::If),
        ("then", tok::Keyword::Then),
        ("else", tok::Keyword::Else),
        ("function", tok::Keyword::Function),
        ("match", tok::Keyword::Match),
        ("with", tok::Keyword::With),
        ("forall", tok::Keyword::Forall),
        ("exists", tok::Keyword::Exists),
    ];

    for (str, kw) in kws {
        let mut lexer = Lexer::new(str);

        let tok = lexer.next().unwrap();
        assert_eq!(lexer[tok], tok::TokenKind::Kw(kw));

        let tok2 = lexer.next().unwrap();
        assert_eq!(lexer[tok2], tok::TokenKind::Eof);

        assert_eq!(lexer.next(), None);

        let (begin, end) = lexer.tokens.get_pos(tok);
        assert_eq!(begin, Position::new(1, 0));
        assert_eq!(end, Position::new(1, str.len()));
    }
}

#[test]
fn scan_ident() {
    let mut lexer = Lexer::new("abc");

    let tok1 = lexer.next().unwrap();
    assert_eq!(lexer[tok1], tok::TokenKind::Ident("abc".into()));

    let tok2 = lexer.next().unwrap();
    assert_eq!(lexer[tok2], tok::TokenKind::Eof);

    assert!(lexer.next().is_none());

    let (begin, end) = lexer.tokens.get_pos(tok1);
    assert_eq!(begin, Position::new(1, 0));
    assert_eq!(end, Position::new(1, 3));
}

#[test]
fn scan_ident_with_numerals() {
    let mut lexer = Lexer::new("abc12");

    let tok1 = lexer.next().unwrap();
    assert_eq!(lexer[tok1], tok::TokenKind::Ident("abc12".into()));

    let tok2 = lexer.next().unwrap();
    assert_eq!(lexer[tok2], tok::TokenKind::Eof);

    assert!(lexer.next().is_none());

    let (begin, end) = lexer.tokens.get_pos(tok1);
    assert_eq!(begin, Position::new(1, 0));
    assert_eq!(end, Position::new(1, 5));
}

#[test]
fn scan_strings() {
    let mut lexer = Lexer::new(r#""abc" "def\"""#);

    let tok1 = lexer.next().unwrap();
    let str1 = tok::ByteString::from_quot(Quotation::Double, r#""abc""#);
    assert_eq!(lexer[tok1], tok::TokenKind::ByteString(str1));

    skip_ws(&mut lexer, tok::Ws::Space { count: 1 });

    let tok2 = lexer.next().unwrap();
    let str2 = tok::ByteString::from_quot(Quotation::Double, r#""def\"""#);
    assert_eq!(lexer[tok2], tok::TokenKind::ByteString(str2));

    let tok3 = lexer.next().unwrap();
    assert_eq!(lexer[tok3], tok::TokenKind::Eof);

    assert_eq!(lexer.next(), None);

    let (begin1, end1) = lexer.tokens.get_pos(tok1);
    assert_eq!(begin1, Position::new(1, 0));
    assert_eq!(end1, Position::new(1, 5));

    let (begin2, end2) = lexer.tokens.get_pos(tok2);
    assert_eq!(begin2, Position::new(1, 6));
    assert_eq!(end2, Position::new(1, 13));
}

#[test]
fn scan_empty_string() {
    let mut lexer = Lexer::new(r#""""#);
    let tok1 = lexer.next().unwrap();
    let str = tok::ByteString::new_bytestring(r#""""#);

    let tok2 = lexer.next().unwrap();
    assert_eq!(lexer[tok2], tok::TokenKind::Eof);

    let (begin, end) = lexer.tokens.get_pos(tok1);
    assert_eq!(lexer[tok1], tok::TokenKind::ByteString(str));
    assert_eq!(begin, Position::new(1, 0));
    assert_eq!(end, Position::new(1, 2));
}

#[test]
fn scan_erroneous_strings() {
    let mut lexer = Lexer::new(r#""abc"#);

    let tok1 = lexer.next().unwrap();
    assert_eq!(lexer[tok1], tok::TokenKind::Unknown(r#""abc"#.into()));

    let tok2 = lexer.next().unwrap();
    assert_eq!(lexer[tok2], tok::TokenKind::Eof);

    let (begin, end) = lexer.tokens.get_pos(tok1);
    assert_eq!(begin, Position::new(1, 0));
    assert_eq!(end, Position::new(1, 4));
}

#[test]
fn identifiers_dont_start_with_digits() {
    let mut lexer = Lexer::new("1abc");

    let tok1 = lexer.next().unwrap();
    assert_eq!(lexer[tok1], tok::TokenKind::Unknown("1abc".into()));

    let tok2 = lexer.next().unwrap();
    assert_eq!(lexer[tok2], tok::TokenKind::Eof);

    let (begin, end) = lexer.tokens.get_pos(tok1);
    assert_eq!(begin, Position::new(1, 0));
    assert_eq!(end, Position::new(1, 4));

    assert!(lexer.next().is_none());
}

#[test]
fn multiple_tokens() {
    let mut lexer = Lexer::new("abc 123");

    let tok1 = lexer.next().unwrap();
    assert_eq!(lexer[tok1], tok::TokenKind::Ident("abc".into()));

    skip_ws(&mut lexer, tok::Ws::Space { count: 1 });

    let tok2 = lexer.next().unwrap();
    assert_eq!(lexer[tok2], tok::TokenKind::Numeral("123".into()));

    let tok3 = lexer.next().unwrap();
    assert_eq!(lexer[tok3], tok::TokenKind::Eof);

    let (begin1, end1) = lexer.tokens.get_pos(tok1);
    assert_eq!(begin1, Position::new(1, 0));
    assert_eq!(end1, Position::new(1, 3));

    let (begin2, end2) = lexer.tokens.get_pos(tok2);
    assert_eq!(begin2, Position::new(1, 4));
    assert_eq!(end2, Position::new(1, 7));
}

#[test]
fn scan_operators() {
    let mut lexer = Lexer::new(". .. .| ~ && ~()");

    let tok1 = lexer.next().unwrap();
    assert_eq!(lexer[tok1], tok::TokenKind::Operator(".".into()));

    skip_ws(&mut lexer, tok::Ws::Space { count: 1 });

    let tok2 = lexer.next().unwrap();
    assert_eq!(lexer[tok2], tok::TokenKind::Operator("..".into()));

    skip_ws(&mut lexer, tok::Ws::Space { count: 1 });

    let tok3 = lexer.next().unwrap();
    assert_eq!(lexer[tok3], tok::TokenKind::Operator(".|".into()));

    skip_ws(&mut lexer, tok::Ws::Space { count: 1 });

    let tok4 = lexer.next().unwrap();
    assert_eq!(lexer[tok4], tok::TokenKind::Operator("~".into()));

    skip_ws(&mut lexer, tok::Ws::Space { count: 1 });

    let tok5 = lexer.next().unwrap();
    assert_eq!(lexer[tok5], tok::TokenKind::Operator("&&".into()));

    skip_ws(&mut lexer, tok::Ws::Space { count: 1 });

    let tok6 = lexer.next().unwrap();
    assert_eq!(lexer[tok6], tok::TokenKind::Operator("~".into()));

    let tok7 = lexer.next().unwrap();
    assert_eq!(
        lexer[tok7],
        tok::TokenKind::Group(tok::Group::Paren(tok::Parity::Opened))
    );

    let tok8 = lexer.next().unwrap();
    assert_eq!(
        lexer[tok8],
        tok::TokenKind::Group(tok::Group::Paren(tok::Parity::Closed))
    );

    let tok9 = lexer.next().unwrap();
    assert_eq!(lexer[tok9], tok::TokenKind::Eof);

    assert!(lexer.next().is_none());

    let (begin1, end1) = lexer.tokens.get_pos(tok1);
    assert_eq!(begin1, Position::new(1, 0));
    assert_eq!(end1, Position::new(1, 1));

    let (begin2, end2) = lexer.tokens.get_pos(tok2);
    assert_eq!(begin2, Position::new(1, 2));
    assert_eq!(end2, Position::new(1, 4));

    let (begin3, end3) = lexer.tokens.get_pos(tok3);
    assert_eq!(begin3, Position::new(1, 5));
    assert_eq!(end3, Position::new(1, 7));

    let (begin4, end4) = lexer.tokens.get_pos(tok4);
    assert_eq!(begin4, Position::new(1, 8));
    assert_eq!(end4, Position::new(1, 9));

    let (begin5, end5) = lexer.tokens.get_pos(tok5);
    assert_eq!(begin5, Position::new(1, 10));
    assert_eq!(end5, Position::new(1, 12));

    let (begin6, end6) = lexer.tokens.get_pos(tok6);
    assert_eq!(begin6, Position::new(1, 13));
    assert_eq!(end6, Position::new(1, 14));

    let (begin7, end7) = lexer.tokens.get_pos(tok7);
    assert_eq!(begin7, Position::new(1, 14));
    assert_eq!(end7, Position::new(1, 15));

    let (begin8, end8) = lexer.tokens.get_pos(tok8);
    assert_eq!(begin8, Position::new(1, 15));
    assert_eq!(end8, Position::new(1, 16));
}

#[test]
fn scan_with_newlines() {
    let mut lexer = Lexer::new("abc def\nghi jkl");

    let tok1 = lexer.next().unwrap();
    assert_eq!(lexer[tok1], tok::TokenKind::Ident("abc".into()));

    skip_ws(&mut lexer, tok::Ws::Space { count: 1 });

    let tok2 = lexer.next().unwrap();
    assert_eq!(lexer[tok2], tok::TokenKind::Ident("def".into()));

    skip_ws(&mut lexer, tok::Ws::Newline { count: 1 });

    let tok3 = lexer.next().unwrap();
    assert_eq!(lexer[tok3], tok::TokenKind::Ident("ghi".into()));

    skip_ws(&mut lexer, tok::Ws::Space { count: 1 });

    let tok4 = lexer.next().unwrap();
    assert_eq!(lexer[tok4], tok::TokenKind::Ident("jkl".into()));

    let tok5 = lexer.next().unwrap();
    assert_eq!(lexer[tok5], tok::TokenKind::Eof);

    assert_eq!(lexer.next(), None);

    let (begin1, end2) = lexer.tokens.get_pos(tok1);
    assert_eq!(begin1, Position::new(1, 0));
    assert_eq!(end2, Position::new(1, 3));

    let (begin2, end2) = lexer.tokens.get_pos(tok2);
    assert_eq!(begin2, Position::new(1, 4));
    assert_eq!(end2, Position::new(1, 7));

    let (begin3, end3) = lexer.tokens.get_pos(tok3);
    assert_eq!(begin3, Position::new(2, 0));
    assert_eq!(end3, Position::new(2, 3));

    let (begin4, end4) = lexer.tokens.get_pos(tok4);
    assert_eq!(begin4, Position::new(2, 4));
    assert_eq!(end4, Position::new(2, 7));
}

// #[test]
// fn scan_indentation() {
//     let mut lexer = Lexer::new("\na\n b\n  c\n d\ne");
//     assert_eq!(lexer.next(), Some(Token::ident("a")));
//     assert_eq!(lexer.next(), Some(Token::indent()));
//     assert_eq!(lexer.next(), Some(Token::ident("b")));
//     assert_eq!(lexer.next(), Some(Token::indent()));
//     assert_eq!(lexer.next(), Some(Token::ident("c")));
//     assert_eq!(lexer.next(), Some(Token::dedent()));
//     assert_eq!(lexer.next(), Some(Token::ident("d")));
//     assert_eq!(lexer.next(), Some(Token::dedent()));
//     assert_eq!(lexer.next(), Some(Token::ident("e")));
//     assert_eq!(lexer.next(), None);
// }

// #[test]
// fn indent_token_are_emitted_only_if_indentation_changes() {
//     let mut lexer = Lexer::new("a\n  b\n  c\n  \td\n  \t  e");
//     assert_eq!(lexer.next(), Some(Token::ident("a")));
//     assert_eq!(lexer.next(), Some(Token::indent()));
//     assert_eq!(lexer.next(), Some(Token::ident("b")));
//     assert_eq!(lexer.next(), Some(Token::ident("c")));
//     assert_eq!(lexer.next(), Some(Token::indent()));
//     assert_eq!(lexer.next(), Some(Token::ident("d")));
//     assert_eq!(lexer.next(), Some(Token::indent()));
//     assert_eq!(lexer.next(), Some(Token::ident("e")));
//     assert_eq!(lexer.next(), None);
// }

// #[test]
// fn no_mixed_indentation() {
//     let mut lexer = Lexer::new("a\n  b\n\t\tc");
//     assert_eq!(lexer.next(), Some(Token::ident("a")));
//     assert_eq!(lexer.next(), Some(Token::indent()));
//     assert_eq!(lexer.next(), Some(Token::ident("b")));
//     assert_eq!(lexer.next(), Some(Token::unknown("\t\t")));
//     assert_eq!(lexer.next(), Some(Token::ident("c")));
//     assert_eq!(lexer.next(), None);
// }
