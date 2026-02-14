use my_regex::Matcher;

macro_rules! define_tokens {
    ($tokenname:ident $tokentypename: ident $($variant:ident $(($associated:ident))?),+) => {
        #[derive(Debug, Clone, PartialEq, Eq, Hash)]
        pub enum $tokenname {
            $($variant $(($associated))?),+
        }

        #[derive(Debug, Clone, PartialEq, Eq, Hash)]
        pub enum $tokentypename {
            $($variant),+
        }

        #[allow(unused_variables, non_snake_case)]
        impl From<&$tokenname> for $tokentypename {
            fn from(value: &$tokenname) -> Self {
                match value {
                    $($tokenname::$variant $(($associated))? => $tokentypename::$variant),+
                }
            }
        }
    }
}

define_tokens!(Token TokenType
    Unit,
    OpenParen,
    CloseParen,
    Int(i64),
    Identifier(String),
    Input,
    Fun,
    Colon,
    Arrow,
    True,
    False,
    If,
    Then,
    Else,
    Let,
    Equal,
    In,
    Deref,
    Ref,
    Assign,
    Begin,
    Semicolon,
    End,
    While,
    Do,
    Fst,
    Snd,
    Inl,
    Inr,
    Case,
    Of,
    Plus,
    Minus,
    Star,
    GreaterThan,
    LessThan,
    GreaterThanOrEqual,
    LessThanOrEqual,
    NotEqual,
    Comma
);
impl Token {
    pub fn are_same_type(&self, other: &Token) -> bool {
        let tokentype: TokenType = self.into();
        tokentype == other.into()
    }

    fn find_match(mut chars: &str) -> Result<Option<(Token, usize)>, ()> {
        // NOTE: exceptional cases: int, assign, identifier, arrow, skip

        let characters = [
            ('?', Token::Input),
            ('(', Token::OpenParen),
            (')', Token::CloseParen),
            (':', Token::Colon),
            ('=', Token::Equal),
            ('!', Token::Deref),
            (';', Token::Semicolon),
            ('+', Token::Plus),
            ('-', Token::Minus),
            ('*', Token::Star),
            ('>', Token::GreaterThan),
            ('<', Token::LessThan),
            (',', Token::Comma),
        ];
        let mut words_with_padding = [
            ("fun", Token::Fun),
            ("true", Token::True),
            ("false", Token::False),
            ("if", Token::If),
            ("then", Token::Then),
            ("else", Token::Else),
            ("let", Token::Let),
            ("in", Token::In),
            ("ref", Token::Ref),
            ("begin", Token::Begin),
            ("end", Token::End),
            ("while", Token::While),
            ("do", Token::Do),
            ("fst", Token::Fst),
            ("snd", Token::Snd),
            ("inl", Token::Inl),
            ("inr", Token::Inr),
            ("case", Token::Case),
            ("of", Token::Of),
        ]
        .into_iter()
        .map(|(word, token)| (Matcher::new(&format!("{word}[ \t\n$]")).unwrap(), token))
        .collect::<Vec<_>>();

        let words_without_padding = [
            (":=", Token::Assign),
            ("->", Token::Arrow),
            (">=", Token::GreaterThanOrEqual),
            ("<=", Token::LessThanOrEqual),
        ]
        .into_iter()
        .map(|(word, token)| (Matcher::new(word).unwrap(), token))
        .collect::<Vec<_>>();

        let words = words_with_padding
            .into_iter()
            .chain(words_without_padding)
            .collect::<Vec<_>>();

        let identregex = Matcher::new("[abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ][abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ]*").unwrap();
        let integerregex = Matcher::new("([+-])?([0123456789][0123456789]*)").unwrap();
        let skipregex = Matcher::new("[ \t\n$][ \t\n$]*").unwrap();

        let mut bytes_taken = 0;
        while !chars.is_empty() {
            // very inefficient but it seems this library doesn't have a way of searching for something
            // at an exact position and this is the first thing that came to mind
            for (wordregex, token) in &words {
                if let Some(matched) = wordregex.longest_match_from_start(chars) {
                    return Ok(Some((token.clone(), bytes_taken + matched.len())));
                }
            }
            if let Some(matched) = identregex.longest_match_from_start(chars) {
                return Ok(Some((
                    Token::Identifier(matched.as_str().to_string()),
                    bytes_taken + matched.len(),
                )));
            } else if let Some(matched) = integerregex.longest_match_from_start(chars) {
                let absolute_value: i64 = matched.parse().unwrap();
                return Ok(Some((
                    Token::Int(absolute_value),
                    bytes_taken + matched.len(),
                )));
            } else if let Some(matched) = skipregex.longest_match_from_start(chars) {
                bytes_taken += matched.len();
                chars = &chars[matched.len()..];
                continue;
            }

            for (character, token) in &characters {
                if chars.chars().next().unwrap() == *character {
                    return Ok(Some((
                        token.clone(),
                        bytes_taken + character.to_string().len(),
                    )));
                }
            }

            return Err(());
        }
        Ok(None)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LexingError {
    UnrecognisedToken,
}

pub fn lex(mut chars: &str) -> Result<Vec<Token>, (Vec<Token>, LexingError)> {
    let mut tokens = vec![];
    while !chars.is_empty() {
        match Token::find_match(chars) {
            Ok(Some((token, bytes_taken))) => {
                chars = &chars[bytes_taken..];
                tokens.push(token);
            }
            Ok(None) => break,
            Err(()) => return Err((tokens, LexingError::UnrecognisedToken)),
        }
    }
    Ok(tokens)
}

#[cfg(test)]
mod test {
    use crate::{LexingError, Token, lex};

    #[test]
    fn lexes_empty() {
        let source = "";
        let expectation = vec![];

        let result = lex(source).unwrap();

        assert_eq!(result, expectation);
    }

    #[test]
    fn lexes_if() {
        let source = "if";
        let expectation = vec![Token::If];

        let result = lex(source).unwrap();

        assert_eq!(result, expectation);
    }

    #[test]
    fn lexes_then() {
        let source = "then";
        let expectation = vec![Token::Then];

        let result = lex(source).unwrap();

        assert_eq!(result, expectation);
    }
    #[test]
    fn lexes_else() {
        let source = "else";
        let expectation = vec![Token::Else];

        let result = lex(source).unwrap();

        assert_eq!(result, expectation);
    }
    #[test]
    fn lexes_equal() {
        let source = "=";
        let expectation = vec![Token::Equal];

        let result = lex(source).unwrap();

        assert_eq!(result, expectation);
    }

    #[test]
    fn lexes_identifier() {
        let source = "potato";
        let expectation = vec![Token::Identifier(String::from("potato"))];

        let result = lex(source).unwrap();

        assert_eq!(result, expectation);
    }

    #[test]
    fn lexes_identifier_starting_with_keyword() {
        let source = "ifpotato";
        let expectation = vec![Token::Identifier(String::from("ifpotato"))];

        let result = lex(source).unwrap();

        assert_eq!(result, expectation);
    }

    #[test]
    fn lexes_int() {
        let source = "456";
        let expectation = vec![Token::Int(456)];

        let result = lex(source).unwrap();

        assert_eq!(result, expectation);
    }
    #[test]
    fn lexes_int_with_plus() {
        let source = "+43";
        let expectation = vec![Token::Int(43)];

        let result = lex(source).unwrap();

        assert_eq!(result, expectation);
    }
    #[test]
    fn lexes_int_with_minus() {
        let source = "-5";
        let expectation = vec![Token::Int(-5)];

        let result = lex(source).unwrap();

        assert_eq!(result, expectation);
    }

    #[test]
    fn lexes_expression() {
        let source = "if a = 4 then   b=-3 else b=4 ";
        let expectation = Ok(vec![
            Token::If,
            Token::Identifier(String::from("a")),
            Token::Equal,
            Token::Int(4),
            Token::Then,
            Token::Identifier(String::from("b")),
            Token::Equal,
            Token::Int(-3),
            Token::Else,
            Token::Identifier(String::from("b")),
            Token::Equal,
            Token::Int(4),
        ]);

        let result = lex(source);

        assert_eq!(result, expectation);
    }

    #[test]
    fn lexes_slang() {
        use Token::*;
        let source = r#"
            let fib (m : int) : int =
                if m = 0 then 1
                else if m = 1 then 1
                else fib (m - 1) + fib (m - 2)
            in
                fib(?)
        "#;
        let fib = Identifier("fib".into());
        let m = Identifier("m".into());
        let int = Identifier("int".into());
        let zero = Int(0);
        let one = Int(1);
        let expectation = Ok(vec![
            Let,
            fib.clone(),
            OpenParen,
            m.clone(),
            Colon,
            int.clone(),
            CloseParen,
            Colon,
            int.clone(),
            Equal,
            If,
            m.clone(),
            Equal,
            zero.clone(),
            Then,
            one.clone(),
            Else,
            If,
            m.clone(),
            Equal,
            one.clone(),
            Then,
            one.clone(),
            Else,
            fib.clone(),
            OpenParen,
            m.clone(),
            Minus,
            one.clone(),
            CloseParen,
            Plus,
            fib.clone(),
            OpenParen,
            m.clone(),
            Minus,
            Int(2),
            CloseParen,
            In,
            fib.clone(),
            OpenParen,
            Input,
            CloseParen,
        ]);

        let result = lex(source);
        assert_eq!(result, expectation);
    }

    #[test]
    fn lexes_slang_2() {
        use Token::*;
        let source = r#"
            let gcd (p : int * int) : int =
                let m : int = fst p in
                let n : int = snd p in
                    if m = n then m 
                    else if m < n then gcd (m, n - m)
                    else gcd(m - n, n)
                in gcd (?, ?)
        "#;
        let gcd = Identifier("gcd".into());
        let open = OpenParen;
        let close = CloseParen;
        let m = Identifier("m".into());
        let n = Identifier("n".into());
        let p = Identifier("p".into());
        let int = Identifier("int".into());
        let expectation = Ok(vec![
            Let,
            gcd.clone(),
            open.clone(),
            p.clone(),
            Colon,
            int.clone(),
            Star,
            int.clone(),
            CloseParen,
            Colon,
            int.clone(),
            Equal,
            Let,
            m.clone(),
            Colon,
            int.clone(),
            Equal,
            Fst,
            p.clone(),
            In,
            Let,
            n.clone(),
            Colon,
            int.clone(),
            Equal,
            Snd,
            p.clone(),
            In,
            If,
            m.clone(),
            Equal,
            n.clone(),
            Then,
            m.clone(),
            Else,
            If,
            m.clone(),
            LessThan,
            n.clone(),
            Then,
            gcd.clone(),
            open.clone(),
            m.clone(),
            Comma,
            n.clone(),
            Minus,
            m.clone(),
            close.clone(),
            Else,
            gcd.clone(),
            open.clone(),
            m.clone(),
            Minus,
            n.clone(),
            Comma,
            n.clone(),
            close.clone(),
            In,
            gcd.clone(),
            open.clone(),
            Input,
            Comma,
            Input,
            close.clone(),
        ]);

        let result = lex(source);
        assert_eq!(result, expectation);
    }
}
